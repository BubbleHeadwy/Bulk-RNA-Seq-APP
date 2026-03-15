source("scripts/common.R")

config <- load_config("config.yaml")
paths <- resolve_paths(config)
ensure_dirs(planned_result_dirs(paths))

results_csv <- file.path(paths$deseq2_dir, "deseq2_results.csv")
annotated_csv <- file.path(paths$annotation_dir, "annotated_deseq2_results.csv")
annotation_report <- file.path(paths$annotation_dir, "annotation_report.md")

assert_no_overwrite(annotated_csv)
assert_no_overwrite(annotation_report)

if (!file.exists(results_csv)) {
  stop("DESeq2 results not found. Run scripts/04_deseq2.R first.", call. = FALSE)
}
if (!requireNamespace("AnnotationDbi", quietly = TRUE) || !requireNamespace("org.Mm.eg.db", quietly = TRUE)) {
  stop("Packages AnnotationDbi and org.Mm.eg.db are required for gene annotation.", call. = FALSE)
}

suppressPackageStartupMessages(library(AnnotationDbi))
suppressPackageStartupMessages(library(org.Mm.eg.db))

extract_gtf_attr <- function(attr_text, key) {
  pattern <- paste0(key, ' "([^"]+)"')
  match <- regexec(pattern, attr_text)
  captured <- regmatches(attr_text, match)
  value <- vapply(captured, function(parts) if (length(parts) >= 2) parts[[2]] else NA_character_, character(1))
  value
}

load_gtf_gene_annotations <- function(gtf_path) {
  con <- if (grepl("\\.gz$", gtf_path, ignore.case = TRUE)) gzfile(gtf_path, open = "rt") else file(gtf_path, open = "rt")
  on.exit(close(con), add = TRUE)

  lines <- readLines(con, warn = FALSE)
  lines <- lines[!startsWith(lines, "#")]
  if (length(lines) == 0) {
    return(data.frame(
      gene_id_clean = character(0),
      gtf_gene_name = character(0),
      gtf_gene_biotype = character(0),
      parent_ensembl = character(0),
      stringsAsFactors = FALSE
    ))
  }

  fields <- strsplit(lines, "\t", fixed = TRUE)
  gene_rows <- vapply(fields, function(parts) length(parts) >= 9 && identical(parts[[3]], "gene"), logical(1))
  fields <- fields[gene_rows]
  if (length(fields) == 0) {
    return(data.frame(
      gene_id_clean = character(0),
      gtf_gene_name = character(0),
      gtf_gene_biotype = character(0),
      parent_ensembl = character(0),
      stringsAsFactors = FALSE
    ))
  }

  attrs <- vapply(fields, `[[`, character(1), 9)
  gene_id <- extract_gtf_attr(attrs, "gene_id")
  gene_name <- extract_gtf_attr(attrs, "gene_name")
  gene_biotype <- extract_gtf_attr(attrs, "gene_biotype")
  parent_gene <- extract_gtf_attr(attrs, "projection_parent_gene")

  annotation_df <- data.frame(
    gene_id_clean = sub("\\.[0-9]+$", "", gene_id),
    gtf_gene_name = gene_name,
    gtf_gene_biotype = gene_biotype,
    parent_ensembl = sub("\\.[0-9]+$", "", parent_gene),
    stringsAsFactors = FALSE
  )

  annotation_df <- annotation_df[!duplicated(annotation_df$gene_id_clean), , drop = FALSE]
  annotation_df
}

res_df <- read.csv(results_csv, stringsAsFactors = FALSE, check.names = FALSE)
if (!"gene_id" %in% names(res_df)) {
  stop("DESeq2 results must contain gene_id.", call. = FALSE)
}
clean_gene_ids <- sub("\\.[0-9]+$", "", res_df$gene_id)
keytype <- detect_gene_id_type(clean_gene_ids)
gtf_annotations <- load_gtf_gene_annotations(paths$gtf_file)

valid_keytypes <- AnnotationDbi::keytypes(org.Mm.eg.db)
direct_mapping_available <- keytype %in% valid_keytypes && any(clean_gene_ids %in% AnnotationDbi::keys(org.Mm.eg.db, keytype = keytype))

symbols <- setNames(rep(NA_character_, length(clean_gene_ids)), clean_gene_ids)
entrez <- setNames(rep(NA_character_, length(clean_gene_ids)), clean_gene_ids)
gene_names <- setNames(rep(NA_character_, length(clean_gene_ids)), clean_gene_ids)
annotation_source <- "gtf_fallback"

if (direct_mapping_available) {
  symbols <- AnnotationDbi::mapIds(org.Mm.eg.db, keys = clean_gene_ids, keytype = keytype, column = "SYMBOL", multiVals = "first")
  entrez <- AnnotationDbi::mapIds(org.Mm.eg.db, keys = clean_gene_ids, keytype = keytype, column = "ENTREZID", multiVals = "first")
  gene_names <- AnnotationDbi::mapIds(org.Mm.eg.db, keys = clean_gene_ids, keytype = keytype, column = "GENENAME", multiVals = "first")
  annotation_source <- "org.Mm.eg.db"
}

if (nrow(gtf_annotations) > 0) {
  idx <- match(clean_gene_ids, gtf_annotations$gene_id_clean)
  gtf_symbol <- gtf_annotations$gtf_gene_name[idx]
  parent_ensembl <- gtf_annotations$parent_ensembl[idx]
  gtf_biotype <- gtf_annotations$gtf_gene_biotype[idx]

  parent_symbol <- AnnotationDbi::mapIds(org.Mm.eg.db, keys = parent_ensembl[!is.na(parent_ensembl)], keytype = "ENSEMBL", column = "SYMBOL", multiVals = "first")
  parent_entrez <- AnnotationDbi::mapIds(org.Mm.eg.db, keys = parent_ensembl[!is.na(parent_ensembl)], keytype = "ENSEMBL", column = "ENTREZID", multiVals = "first")
  parent_gene_name <- AnnotationDbi::mapIds(org.Mm.eg.db, keys = parent_ensembl[!is.na(parent_ensembl)], keytype = "ENSEMBL", column = "GENENAME", multiVals = "first")

  fallback_symbol <- ifelse(!is.na(gtf_symbol) & nzchar(gtf_symbol), gtf_symbol, unname(parent_symbol[parent_ensembl]))
  fallback_entrez <- unname(parent_entrez[parent_ensembl])
  fallback_gene_name <- unname(parent_gene_name[parent_ensembl])

  missing_symbol <- is.na(symbols) | !nzchar(as.character(symbols))
  missing_entrez <- is.na(entrez) | !nzchar(as.character(entrez))
  missing_gene_name <- is.na(gene_names) | !nzchar(as.character(gene_names))

  symbols[missing_symbol] <- fallback_symbol[missing_symbol]
  entrez[missing_entrez] <- fallback_entrez[missing_entrez]
  gene_names[missing_gene_name] <- fallback_gene_name[missing_gene_name]
} else {
  gtf_biotype <- rep(NA_character_, length(clean_gene_ids))
  parent_ensembl <- rep(NA_character_, length(clean_gene_ids))
}

annotated_df <- res_df
annotated_df$gene_id_clean <- clean_gene_ids
annotated_df$gene_id_type <- keytype
annotated_df$annotation_source <- annotation_source
annotated_df$parent_ensembl <- parent_ensembl
annotated_df$gtf_gene_biotype <- gtf_biotype
annotated_df$SYMBOL <- unname(symbols[clean_gene_ids])
annotated_df$ENTREZID <- unname(entrez[clean_gene_ids])
annotated_df$GENENAME <- unname(gene_names[clean_gene_ids])

write_csv_safe(annotated_df, annotated_csv, row.names = FALSE)
write_lines_safe(c(
  "# Annotation Report",
  "",
  paste("- Generated:", timestamp_now()),
  paste("- Gene ID type:", keytype),
  paste("- Annotation source:", annotation_source),
  paste("- Input rows:", nrow(res_df)),
  paste("- Rows with SYMBOL annotation:", sum(!is.na(annotated_df$SYMBOL))),
  paste("- Rows with ENTREZID annotation:", sum(!is.na(annotated_df$ENTREZID))),
  paste("- Output:", annotated_csv)
), annotation_report)
