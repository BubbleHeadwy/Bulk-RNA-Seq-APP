source("scripts/common.R")

config <- load_config("config.yaml")
paths <- resolve_paths(config)
ensure_dirs(planned_result_dirs(paths))

annotated_csv <- file.path(paths$annotation_dir, "annotated_deseq2_results.csv")
go_csv <- file.path(paths$enrichment_dir, "go_enrichment_results.csv")
go_report <- file.path(paths$enrichment_dir, "go_enrichment_report.md")

assert_no_overwrite(go_csv)
assert_no_overwrite(go_report)

if (!file.exists(annotated_csv)) {
  stop("Annotated DESeq2 results not found. Run scripts/06_gene_annotation.R first.", call. = FALSE)
}
if (!requireNamespace("clusterProfiler", quietly = TRUE) || !requireNamespace("org.Mm.eg.db", quietly = TRUE)) {
  stop("Packages clusterProfiler and org.Mm.eg.db are required for GO enrichment.", call. = FALSE)
}

suppressPackageStartupMessages(library(clusterProfiler))
suppressPackageStartupMessages(library(org.Mm.eg.db))

annotated_df <- read.csv(annotated_csv, stringsAsFactors = FALSE, check.names = FALSE)
required_columns <- c("ENTREZID", "padj", "log2FoldChange")
missing_columns <- setdiff(required_columns, names(annotated_df))
if (length(missing_columns) > 0) {
  stop("Annotated results missing required columns for GO enrichment: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

sig_df <- annotated_df[
  !is.na(annotated_df$ENTREZID) &
    !is.na(annotated_df$padj) &
    annotated_df$padj < 0.05 &
    !is.na(annotated_df$log2FoldChange) &
    abs(annotated_df$log2FoldChange) >= 1,
  ,
  drop = FALSE
]

if (nrow(sig_df) == 0) {
  write_csv_safe(data.frame(), go_csv, row.names = FALSE)
  write_lines_safe(c(
    "# GO Enrichment Report",
    "",
    paste("- Generated:", timestamp_now()),
    "- Significant genes used: 0",
    "- Enriched terms: 0",
    "- Status: No genes passed the padj < 0.05 and |log2FoldChange| >= 1 thresholds.",
    paste("- Output:", go_csv)
  ), go_report)
  quit(save = "no", status = 0)
}

ego <- clusterProfiler::enrichGO(
  gene = unique(sig_df$ENTREZID),
  OrgDb = org.Mm.eg.db,
  keyType = "ENTREZID",
  ont = "BP",
  pAdjustMethod = "BH",
  readable = TRUE
)

go_df <- as.data.frame(ego)
if (nrow(go_df) == 0) {
  write_csv_safe(go_df, go_csv, row.names = FALSE)
  write_lines_safe(c(
    "# GO Enrichment Report",
    "",
    paste("- Generated:", timestamp_now()),
    paste("- Significant genes used:", length(unique(sig_df$ENTREZID))),
    "- Enriched terms: 0",
    "- Status: GO enrichment ran successfully but returned no enriched terms.",
    paste("- Output:", go_csv)
  ), go_report)
  quit(save = "no", status = 0)
}

write_csv_safe(go_df, go_csv, row.names = FALSE)
write_lines_safe(c(
  "# GO Enrichment Report",
  "",
  paste("- Generated:", timestamp_now()),
  paste("- Significant genes used:", length(unique(sig_df$ENTREZID))),
  paste("- Enriched terms:", nrow(go_df)),
  paste("- Output:", go_csv)
), go_report)
