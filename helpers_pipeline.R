source("scripts/common.R")

viz_repo_root <- function() {
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

viz_project_config <- function() {
  config_path <- file.path(viz_repo_root(), "config.yaml")
  if (!file.exists(config_path)) {
    return(list())
  }
  load_config(config_path)
}

viz_results_root <- function() {
  normalizePath(file.path(viz_repo_root(), "results"), winslash = "/", mustWork = FALSE)
}

viz_default_output_dir <- function() {
  normalizePath(
    file.path(viz_results_root(), paste0("visualization_run_", format(Sys.time(), "%Y%m%d_%H%M%S"))),
    winslash = "/",
    mustWork = FALSE
  )
}

viz_read_metadata <- function(sample_sheet) {
  df <- read.csv(sample_sheet, stringsAsFactors = FALSE, check.names = FALSE)
  validate_metadata(df, require_design = FALSE)
}

subset_bundle <- function(bundle, selected_sample_ids = NULL) {
  if (is.null(bundle) || is.null(bundle$metadata)) {
    return(bundle)
  }
  ids <- unique(as.character(selected_sample_ids %||% character(0)))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) {
    return(bundle)
  }

  metadata_df <- bundle$metadata[bundle$metadata$sample_id %in% ids, , drop = FALSE]

  subset_matrix_like <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    keep_cols <- intersect(c("gene_id", metadata_df$sample_id), names(df))
    df[, keep_cols, drop = FALSE]
  }

  out <- bundle
  out$metadata_all <- bundle$metadata
  out$metadata <- metadata_df
  out$selected_sample_ids <- metadata_df$sample_id
  out$counts <- subset_matrix_like(bundle$counts)
  out$normalized <- subset_matrix_like(bundle$normalized)
  out
}

compute_selected_pca_analysis <- function(bundle, selected_sample_ids) {
  sub_bundle <- subset_bundle(bundle, selected_sample_ids)
  pca_df <- compute_pca_df(sub_bundle$normalized, sub_bundle$metadata)
  list(
    metadata = sub_bundle$metadata,
    normalized = sub_bundle$normalized,
    pca = pca_df,
    pca_kind = "all_samples"
  )
}

load_results_bundle <- function(results_dir, sample_sheet, gtf_file = "", selected_sample_ids = NULL) {
  check <- validate_results_bundle(results_dir, sample_sheet)
  if (!isTRUE(check$ok)) {
    stop(check$message, call. = FALSE)
  }

  metadata_df <- viz_read_metadata(sample_sheet)
  bundle <- list(
    source_mode = "load_results",
    results_dir = normalizePath(results_dir, winslash = "/", mustWork = FALSE),
    sample_sheet = normalizePath(sample_sheet, winslash = "/", mustWork = FALSE),
    gtf_file = if (nzchar(gtf_file)) normalizePath(gtf_file, winslash = "/", mustWork = FALSE) else "",
    metadata = metadata_df,
    counts = safe_read_csv(check$details$counts),
    normalized = safe_read_csv(check$details$normalized),
    deseq = safe_read_csv(check$details$deseq),
    annotated = safe_read_csv(check$details$annotated),
    go = safe_read_csv(check$details$go)
  )
  subset_bundle(bundle, selected_sample_ids)
}

load_gtf_gene_annotations_app <- function(gtf_path) {
  if (!nzchar(gtf_path) || !file.exists(gtf_path)) {
    return(data.frame(
      gene_id_clean = character(0),
      gtf_gene_name = character(0),
      gtf_gene_biotype = character(0),
      parent_ensembl = character(0),
      stringsAsFactors = FALSE
    ))
  }

  extract_gtf_attr <- function(attr_text, key) {
    pattern <- paste0(key, ' "([^"]+)"')
    match <- regexec(pattern, attr_text)
    captured <- regmatches(attr_text, match)
    vapply(captured, function(parts) if (length(parts) >= 2) parts[[2]] else NA_character_, character(1))
  }

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
  annotation_df <- data.frame(
    gene_id_clean = sub("\\.[0-9]+$", "", extract_gtf_attr(attrs, "gene_id")),
    gtf_gene_name = extract_gtf_attr(attrs, "gene_name"),
    gtf_gene_biotype = extract_gtf_attr(attrs, "gene_biotype"),
    parent_ensembl = sub("\\.[0-9]+$", "", extract_gtf_attr(attrs, "projection_parent_gene")),
    stringsAsFactors = FALSE
  )
  annotation_df[!duplicated(annotation_df$gene_id_clean), , drop = FALSE]
}

annotate_results_df <- function(res_df, gtf_file = "") {
  if (!requireNamespace("AnnotationDbi", quietly = TRUE) || !requireNamespace("org.Mm.eg.db", quietly = TRUE)) {
    res_df$SYMBOL <- NA_character_
    res_df$ENTREZID <- NA_character_
    res_df$GENENAME <- NA_character_
    return(res_df)
  }

  suppressPackageStartupMessages(library(AnnotationDbi))
  suppressPackageStartupMessages(library(org.Mm.eg.db))

  safe_map_ids <- function(keys, keytype, column) {
    clean_keys <- unique(trimws(as.character(keys)))
    clean_keys <- clean_keys[!is.na(clean_keys) & nzchar(clean_keys)]
    if (length(clean_keys) == 0) {
      return(setNames(character(0), character(0)))
    }
    suppressWarnings(
      AnnotationDbi::mapIds(
        org.Mm.eg.db,
        keys = clean_keys,
        keytype = keytype,
        column = column,
        multiVals = "first"
      )
    )
  }

  clean_gene_ids <- sub("\\.[0-9]+$", "", res_df$gene_id)
  keytype <- detect_gene_id_type(clean_gene_ids)
  gtf_annotations <- load_gtf_gene_annotations_app(gtf_file)
  valid_keytypes <- AnnotationDbi::keytypes(org.Mm.eg.db)

  direct_mapping_available <- FALSE
  if (keytype %in% valid_keytypes) {
    direct_mapping_available <- tryCatch({
      key_space <- AnnotationDbi::keys(org.Mm.eg.db, keytype = keytype)
      any(clean_gene_ids %in% key_space)
    }, error = function(e) FALSE)
  }

  symbols <- setNames(rep(NA_character_, length(clean_gene_ids)), clean_gene_ids)
  entrez <- setNames(rep(NA_character_, length(clean_gene_ids)), clean_gene_ids)
  gene_names <- setNames(rep(NA_character_, length(clean_gene_ids)), clean_gene_ids)

  if (isTRUE(direct_mapping_available)) {
    symbols <- safe_map_ids(clean_gene_ids, keytype = keytype, column = "SYMBOL")
    entrez <- safe_map_ids(clean_gene_ids, keytype = keytype, column = "ENTREZID")
    gene_names <- safe_map_ids(clean_gene_ids, keytype = keytype, column = "GENENAME")
  }

  if (nrow(gtf_annotations) > 0) {
    idx <- match(clean_gene_ids, gtf_annotations$gene_id_clean)
    gtf_symbol <- gtf_annotations$gtf_gene_name[idx]
    gtf_biotype <- gtf_annotations$gtf_gene_biotype[idx]
    parent_ensembl <- gtf_annotations$parent_ensembl[idx]
    parent_symbol <- safe_map_ids(parent_ensembl, keytype = "ENSEMBL", column = "SYMBOL")
    parent_entrez <- safe_map_ids(parent_ensembl, keytype = "ENSEMBL", column = "ENTREZID")
    parent_gene_name <- safe_map_ids(parent_ensembl, keytype = "ENSEMBL", column = "GENENAME")

    fallback_symbol <- ifelse(!is.na(gtf_symbol) & nzchar(gtf_symbol), gtf_symbol, unname(parent_symbol[parent_ensembl]))
    fallback_entrez <- unname(parent_entrez[parent_ensembl])
    fallback_gene_name <- unname(parent_gene_name[parent_ensembl])

    missing_symbol <- is.na(symbols) | !nzchar(as.character(symbols))
    missing_entrez <- is.na(entrez) | !nzchar(as.character(entrez))
    missing_gene_name <- is.na(gene_names) | !nzchar(as.character(gene_names))

    symbols[missing_symbol] <- fallback_symbol[missing_symbol]
    entrez[missing_entrez] <- fallback_entrez[missing_entrez]
    gene_names[missing_gene_name] <- fallback_gene_name[missing_gene_name]

    res_df$gtf_gene_biotype <- gtf_biotype
    res_df$parent_ensembl <- parent_ensembl
  } else {
    res_df$gtf_gene_biotype <- NA_character_
    res_df$parent_ensembl <- NA_character_
  }

  res_df$gene_id_clean <- clean_gene_ids
  res_df$gene_id_type <- keytype
  res_df$SYMBOL <- unname(symbols[clean_gene_ids])
  res_df$ENTREZID <- unname(entrez[clean_gene_ids])
  res_df$GENENAME <- unname(gene_names[clean_gene_ids])
  res_df
}

run_go_enrichment_app <- function(annotated_df, lfc_threshold = 1, padj_threshold = 0.05) {
  if (!requireNamespace("clusterProfiler", quietly = TRUE) || !requireNamespace("org.Mm.eg.db", quietly = TRUE)) {
    return(list(data = data.frame(), message = "clusterProfiler or org.Mm.eg.db is missing; GO enrichment is unavailable."))
  }
  if (!all(c("ENTREZID", "padj", "log2FoldChange") %in% names(annotated_df))) {
    return(list(data = data.frame(), message = "Missing required columns for GO enrichment."))
  }

  sig_df <- annotated_df[
    !is.na(annotated_df$ENTREZID) &
      !is.na(annotated_df$padj) &
      annotated_df$padj < padj_threshold &
      !is.na(annotated_df$log2FoldChange) &
      abs(annotated_df$log2FoldChange) >= lfc_threshold,
    ,
    drop = FALSE
  ]
  if (nrow(sig_df) == 0) {
    return(list(data = data.frame(), message = "No significant genes available for GO enrichment."))
  }

  suppressPackageStartupMessages(library(clusterProfiler))
  suppressPackageStartupMessages(library(org.Mm.eg.db))
  ego <- tryCatch(
    clusterProfiler::enrichGO(
      gene = unique(sig_df$ENTREZID),
      OrgDb = org.Mm.eg.db,
      keyType = "ENTREZID",
      ont = "BP",
      pAdjustMethod = "BH",
      readable = TRUE
    ),
    error = function(e) e
  )
  if (inherits(ego, "error")) {
    return(list(data = data.frame(), message = paste("GO enrichment failed:", conditionMessage(ego))))
  }
  go_df <- as.data.frame(ego)
  if (nrow(go_df) == 0) {
    return(list(data = go_df, message = "GO enrichment finished but returned no significant terms."))
  }
  list(data = go_df, message = paste0("GO enrichment completed with ", nrow(go_df), " significant terms."))
}

compute_pca_df <- function(normalized_df, metadata_df) {
  sample_columns <- intersect(metadata_df$sample_id, names(normalized_df))
  if (length(sample_columns) < 2) {
    return(data.frame())
  }
  norm_matrix <- as.matrix(normalized_df[, sample_columns, drop = FALSE])
  rownames(norm_matrix) <- normalized_df$gene_id
  variance <- apply(norm_matrix, 1, stats::var, na.rm = TRUE)
  variance[is.na(variance)] <- 0
  selected <- order(variance, decreasing = TRUE)[seq_len(min(500, length(variance)))]
  pca <- prcomp(t(log1p(norm_matrix[selected, , drop = FALSE])), scale. = TRUE)
  explained <- (pca$sdev^2) / sum(pca$sdev^2)
  out <- data.frame(
    sample_id = rownames(pca$x),
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    metadata_df[match(rownames(pca$x), metadata_df$sample_id), setdiff(names(metadata_df), c("sample_id", "bam_file")), drop = FALSE],
    stringsAsFactors = FALSE
  )
  attr(out, "percent_var") <- explained
  out
}

run_contrast_analysis <- function(bundle, selected_sample_ids, settings, log_callback = function(...) {}, progress_callback = function(...) {}) {
  if (!requireNamespace("DESeq2", quietly = TRUE)) {
    stop("DESeq2 is missing. Install dependencies first.", call. = FALSE)
  }

  log_callback("Preparing selected sample subset.")
  progress_callback(0.1, "Preparing selected sample subset")
  sub_bundle <- subset_bundle(bundle, selected_sample_ids)
  metadata_df <- sub_bundle$metadata
  count_df <- sub_bundle$counts
  if (is.null(metadata_df) || is.null(count_df) || nrow(metadata_df) == 0) {
    stop("No samples are available for contrast analysis.", call. = FALSE)
  }

  apply_optional_filter <- function(df, column, selected_value) {
    if (!column %in% names(df)) {
      return(df)
    }
    value <- trimws(as.character(selected_value %||% ""))
    if (!nzchar(value)) {
      return(df)
    }
    # Only filter when the selected value is an actual level in data.
    # This avoids locale-specific "All"/"全部" sentinel mismatches.
    available <- unique(trimws(as.character(df[[column]])))
    if (!(value %in% available)) {
      return(df)
    }
    df[df[[column]] == value, , drop = FALSE]
  }

  metadata_df <- apply_optional_filter(metadata_df, "genotype", settings$genotype_filter)
  metadata_df <- apply_optional_filter(metadata_df, "condition", settings$condition_filter)

  groups <- unique(metadata_df[[settings$contrast_factor]])
  if (!all(c(settings$experiment_group, settings$control_group) %in% groups)) {
    stop("Selected groups are not present under current filters.", call. = FALSE)
  }
  if (identical(settings$experiment_group, settings$control_group)) {
    stop("Experiment group and control group cannot be the same.", call. = FALSE)
  }

  metadata_df <- metadata_df[metadata_df[[settings$contrast_factor]] %in% c(settings$experiment_group, settings$control_group), , drop = FALSE]
  if (nrow(metadata_df) < 2) {
    stop("Not enough samples for differential expression under current filters.", call. = FALSE)
  }
  if (isTRUE(settings$use_batch) && !"batch" %in% names(metadata_df)) {
    stop("Batch correction is enabled but sample sheet has no batch column.", call. = FALSE)
  }

  log_callback("Validating design formula.")
  progress_callback(0.2, "Validating design formula")
  formula_check <- validate_formula_string(settings$design_formula, metadata_df)
  if (!isTRUE(formula_check$ok)) {
    stop(formula_check$message, call. = FALSE)
  }

  sample_columns <- metadata_df$sample_id
  count_matrix <- as.matrix(count_df[, c("gene_id", sample_columns), drop = FALSE][, sample_columns, drop = FALSE])
  rownames(count_matrix) <- count_df$gene_id
  storage.mode(count_matrix) <- "integer"
  for (col in setdiff(names(metadata_df), c("sample_id", "bam_file"))) {
    metadata_df[[col]] <- as.factor(metadata_df[[col]])
  }
  rownames(metadata_df) <- metadata_df$sample_id

  log_callback("Fitting DESeq2 model.")
  progress_callback(0.35, "Fitting DESeq2 model")
  suppressPackageStartupMessages(library(DESeq2))
  dds <- DESeq2::DESeqDataSetFromMatrix(countData = count_matrix, colData = metadata_df, design = formula_check$formula)
  dds <- DESeq2::DESeq(dds, quiet = TRUE)

  log_callback("Extracting normalized matrix and differential results.")
  progress_callback(0.6, "Extracting normalized matrix and differential results")
  normalized <- DESeq2::counts(dds, normalized = TRUE)
  normalized_df <- data.frame(gene_id = rownames(normalized), normalized, row.names = NULL, check.names = FALSE)
  colnames(normalized_df) <- c("gene_id", sample_columns)

  res <- DESeq2::results(dds, contrast = c(settings$contrast_factor, settings$experiment_group, settings$control_group))
  res_df <- data.frame(gene_id = rownames(res), as.data.frame(res), row.names = NULL, check.names = FALSE)

  log_callback("Annotating genes.")
  progress_callback(0.75, "Annotating genes")
  annotated_df <- annotate_results_df(res_df, sub_bundle$gtf_file %||% "")
  annotated_df$display_label <- ifelse(is.na(annotated_df$SYMBOL) | !nzchar(annotated_df$SYMBOL), annotated_df$gene_id, annotated_df$SYMBOL)
  annotated_df$significant <- !is.na(annotated_df$padj) &
    annotated_df$padj < settings$padj_threshold &
    !is.na(annotated_df$log2FoldChange) &
    abs(annotated_df$log2FoldChange) >= settings$lfc_threshold

  log_callback("Running GO enrichment and contrast PCA.")
  progress_callback(0.88, "Running GO enrichment and contrast PCA")
  go_result <- run_go_enrichment_app(annotated_df, lfc_threshold = settings$lfc_threshold, padj_threshold = settings$padj_threshold)
  pca_df <- compute_pca_df(normalized_df, metadata_df)

  progress_callback(1.0, "Current 1v1 contrast analysis completed")
  log_callback("Current 1v1 contrast analysis completed.")
  list(
    metadata = metadata_df,
    normalized = normalized_df,
    deseq = res_df,
    annotated = annotated_df,
    significant = annotated_df[annotated_df$significant, , drop = FALSE],
    go = go_result$data,
    go_message = go_result$message,
    pca = pca_df,
    pca_kind = "contrast"
  )
}

build_runtime_paths <- function(output_dir) {
  list(
    output_dir = output_dir,
    inventory_dir = file.path(output_dir, "01_inventory"),
    counts_dir = file.path(output_dir, "02_counts"),
    deseq2_dir = file.path(output_dir, "03_deseq2"),
    qc_dir = file.path(output_dir, "04_qc"),
    plots_dir = file.path(output_dir, "05_plots"),
    annotation_dir = file.path(output_dir, "06_annotation"),
    enrichment_dir = file.path(output_dir, "07_enrichment"),
    logs_dir = file.path(output_dir, "08_logs"),
    tmp_dir = file.path(output_dir, "09_tmp")
  )
}

run_precompute_pipeline <- function(bam_dir, gtf_file, sample_sheet, output_dir, selected_sample_ids, log_callback = function(...) {}, progress_callback = function(...) {}) {
  validate_result <- validate_bam_inputs(bam_dir, sample_sheet, include_batch = FALSE)
  if (!isTRUE(validate_result$ok)) {
    stop(validate_result$message, call. = FALSE)
  }
  gtf_check <- validate_gtf_file(gtf_file)
  if (!isTRUE(gtf_check$ok)) {
    stop(gtf_check$message, call. = FALSE)
  }
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE)) > 0) {
    stop("Target output directory already contains files. Choose an empty output directory.", call. = FALSE)
  }

  runtime_paths <- build_runtime_paths(output_dir)
  ensure_dirs(unname(runtime_paths))

  metadata_df <- apply_sample_selection(validate_result$metadata, selected_sample_ids)
  if (nrow(metadata_df) < 2) {
    stop("Select at least 2 samples before starting new analysis.", call. = FALSE)
  }

  inventory_files <- validate_result$bam_table
  valid_candidates <- inventory_files[inventory_files$is_bam & !inventory_files$is_bad & inventory_files$bai_exists, c("sample_id", "bam_file", "bam_path", "bai_path"), drop = FALSE]
  valid_candidates <- valid_candidates[valid_candidates$sample_id %in% metadata_df$sample_id, , drop = FALSE]

  write_tsv_safe(valid_candidates, file.path(runtime_paths$inventory_dir, "candidate_samples.tsv"), row.names = FALSE)
  write_tsv_safe(inventory_files, file.path(runtime_paths$inventory_dir, "inventory_files.tsv"), row.names = FALSE)
  write.csv(metadata_df, file.path(output_dir, "selected_samples.csv"), row.names = FALSE, quote = TRUE, na = "")

  metadata_df$bam_path <- file.path(bam_dir, metadata_df$bam_file)
  config <- viz_project_config()
  config$count_method <- "Rsubread"
  config$bam_dir <- bam_dir
  config$gtf_file <- gtf_file
  samtools_path <- command_path("samtools", config = config, config_field = "samtools_path")

  log_callback("Scanning selected sample files.")
  progress_callback(0.1, "Scanning selected BAM files")

  pairing_info <- do.call(rbind, lapply(seq_len(nrow(metadata_df)), function(i) {
    pairing <- infer_pairing_status(metadata_df$bam_path[[i]], samtools_path)
    data.frame(
      sample_id = metadata_df$sample_id[[i]],
      bam_file = metadata_df$bam_file[[i]],
      pairing_status = pairing$status,
      pairing_ratio = pairing$ratio,
      pairing_reason = pairing$reason,
      stringsAsFactors = FALSE
    )
  }))
  if (any(pairing_info$pairing_status %in% c("unknown", "mixed"))) {
    stop("BAM pairing status is uncertain. Check BAM integrity before counting.", call. = FALSE)
  }

  paired_flags <- unique(pairing_info$pairing_status == "paired_end")
  if (length(paired_flags) != 1) {
    stop("Mixed single-end and paired-end samples are not supported in one run.", call. = FALSE)
  }

  engine <- select_count_engine(config)
  if (is.na(engine$engine)) {
    stop("No counting engine available. Install Rsubread or featureCounts.", call. = FALSE)
  }
  annotation_file <- prepare_annotation_for_counting(gtf_file, engine$engine, runtime_paths$tmp_dir)
  strand_info <- list(status = "unknown", strand_specific = 0L, note = "Defaulting to unstranded mode.")

  log_callback("Generating raw count matrix.")
  progress_callback(0.35, "Running featureCounts / Rsubread counting")
  if (engine$engine == "Rsubread") {
    suppressPackageStartupMessages(library(Rsubread))
    fc <- Rsubread::featureCounts(
      files = metadata_df$bam_path,
      annot.ext = annotation_file,
      isGTFAnnotationFile = TRUE,
      GTF.featureType = "exon",
      GTF.attrType = "gene_id",
      useMetaFeatures = TRUE,
      isPairedEnd = isTRUE(paired_flags[[1]]),
      strandSpecific = strand_info$strand_specific,
      nthreads = 1
    )
    counts_df <- data.frame(gene_id = rownames(fc$counts), fc$counts, row.names = NULL, check.names = FALSE)
    colnames(counts_df) <- c("gene_id", metadata_df$sample_id)
    summary_df <- data.frame(status = rownames(fc$stat), fc$stat, row.names = NULL, check.names = FALSE)
    colnames(summary_df) <- c("status", metadata_df$sample_id)
  } else {
    raw_output <- file.path(runtime_paths$tmp_dir, "featurecounts_raw.txt")
    args <- c("-a", annotation_file, "-o", raw_output, "-T", "1", "-t", "exon", "-g", "gene_id", "-s", as.character(strand_info$strand_specific))
    if (isTRUE(paired_flags[[1]])) args <- c(args, "-p")
    args <- c(args, metadata_df$bam_path)
    result <- run_command(engine$command, args)
    if (result$status != 0L) {
      stop("featureCounts failed:", paste(result$output, collapse = "\n"), call. = FALSE)
    }
    raw_counts <- read.delim(raw_output, comment.char = "#", stringsAsFactors = FALSE, check.names = FALSE)
    summary_raw <- read.delim(paste0(raw_output, ".summary"), stringsAsFactors = FALSE, check.names = FALSE)
    counts_only <- raw_counts[, metadata_df$bam_path, drop = FALSE]
    colnames(counts_only) <- metadata_df$sample_id
    counts_df <- data.frame(gene_id = raw_counts$Geneid, counts_only, check.names = FALSE)
    summary_only <- summary_raw[, metadata_df$bam_path, drop = FALSE]
    colnames(summary_only) <- metadata_df$sample_id
    summary_df <- data.frame(status = summary_raw$Status, summary_only, check.names = FALSE)
  }

  write_csv_safe(counts_df, file.path(runtime_paths$counts_dir, "raw_gene_counts.csv"), row.names = FALSE)
  write_csv_safe(summary_df, file.path(runtime_paths$counts_dir, "featurecounts_summary.csv"), row.names = FALSE)

  log_callback("Calculating normalized matrix and generating all-sample PCA.")
  progress_callback(0.7, "Calculating normalized matrix")
  if (!requireNamespace("DESeq2", quietly = TRUE)) {
    stop("DESeq2 is missing. Install dependencies first.", call. = FALSE)
  }

  suppressPackageStartupMessages(library(DESeq2))
  count_matrix <- as.matrix(counts_df[, metadata_df$sample_id, drop = FALSE])
  rownames(count_matrix) <- counts_df$gene_id
  storage.mode(count_matrix) <- "integer"
  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = count_matrix,
    colData = data.frame(row.names = metadata_df$sample_id, sample_id = metadata_df$sample_id),
    design = ~1
  )
  dds <- DESeq2::estimateSizeFactors(dds)
  normalized <- DESeq2::counts(dds, normalized = TRUE)
  normalized_df <- data.frame(gene_id = rownames(normalized), normalized, row.names = NULL, check.names = FALSE)
  colnames(normalized_df) <- c("gene_id", metadata_df$sample_id)
  write_csv_safe(normalized_df, file.path(runtime_paths$deseq2_dir, "normalized_counts.csv"), row.names = FALSE)

  pca_df <- compute_pca_df(normalized_df, metadata_df)
  progress_callback(1.0, "All-sample PCA generated")
  log_callback("Precompute pipeline completed. All-sample PCA is ready.")

  list(
    source_mode = "run_analysis",
    results_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    sample_sheet = normalizePath(sample_sheet, winslash = "/", mustWork = FALSE),
    gtf_file = if (nzchar(gtf_file)) normalizePath(gtf_file, winslash = "/", mustWork = FALSE) else "",
    metadata = metadata_df[, setdiff(names(metadata_df), "bam_path"), drop = FALSE],
    counts = counts_df,
    normalized = normalized_df,
    deseq = NULL,
    annotated = NULL,
    go = NULL,
    pca = pca_df
  )
}

