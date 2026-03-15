source("scripts/common.R")

args <- parse_cli_args(commandArgs(trailingOnly = TRUE))
config <- load_config("config.yaml")
paths <- resolve_paths(config)
ensure_dirs(planned_result_dirs(paths))

counts_path <- file.path(paths$counts_dir, "raw_gene_counts.csv")
design_candidates_tsv <- file.path(paths$deseq2_dir, "design_candidates.tsv")
design_candidates_md <- file.path(paths$deseq2_dir, "design_candidates.md")
available_results_tsv <- file.path(paths$deseq2_dir, "available_results.tsv")
normalized_counts_csv <- file.path(paths$deseq2_dir, "normalized_counts.csv")
deseq_results_csv <- file.path(paths$deseq2_dir, "deseq2_results.csv")
run_report_md <- file.path(paths$deseq2_dir, "deseq2_run_report.md")

if (!file.exists(counts_path)) {
  stop("Raw count matrix not found. Run scripts/03_counts_from_bam.R first.", call. = FALSE)
}
if (file.exists(design_candidates_tsv) || file.exists(design_candidates_md)) {
  stop("Design candidate outputs already exist. Remove existing files before rerunning DESeq2 candidate generation.", call. = FALSE)
}

metadata_df <- read.csv(paths$metadata, stringsAsFactors = FALSE, check.names = FALSE)
metadata_df <- validate_metadata(metadata_df, require_design = TRUE)
count_df <- read.csv(counts_path, stringsAsFactors = FALSE, check.names = FALSE)

if (!"gene_id" %in% names(count_df)) {
  stop("Count matrix must contain a gene_id column.", call. = FALSE)
}

count_sample_columns <- setdiff(names(count_df), "gene_id")
missing_samples <- setdiff(metadata_df$sample_id, count_sample_columns)
if (length(missing_samples) > 0) {
  stop("Count matrix is missing metadata samples: ", paste(missing_samples, collapse = ", "), call. = FALSE)
}
sample_columns <- metadata_df$sample_id
count_df <- count_df[, c("gene_id", sample_columns), drop = FALSE]
metadata_df <- metadata_df[match(sample_columns, metadata_df$sample_id), , drop = FALSE]

candidates_df <- generate_design_candidates(metadata_df)
if (nrow(candidates_df) == 0) {
  stop("No DESeq2 design candidates could be generated from metadata.", call. = FALSE)
}
write_tsv_safe(candidates_df, design_candidates_tsv, row.names = FALSE)
write_lines_safe(c(
  "# Design Candidates",
  "",
  paste("- Generated:", timestamp_now()),
  "",
  markdown_table(candidates_df)
), design_candidates_md)

selected_design <- trim_scalar(args$design %||% "")
if (!nzchar(selected_design)) {
  stop("Design candidates were generated. Re-run with --design '<formula>' after user confirmation.", call. = FALSE)
}

if (file.exists(available_results_tsv) || file.exists(normalized_counts_csv) || file.exists(deseq_results_csv) || file.exists(run_report_md)) {
  stop("DESeq2 outputs already exist. Remove existing files before rerunning.", call. = FALSE)
}

if (!requireNamespace("DESeq2", quietly = TRUE)) {
  stop("Package DESeq2 is required for differential expression analysis.", call. = FALSE)
}

suppressPackageStartupMessages(library(DESeq2))
design_formula <- validate_design_formula(selected_design, metadata_df)
count_matrix <- as.matrix(count_df[, sample_columns, drop = FALSE])
storage.mode(count_matrix) <- "integer"
rownames(count_matrix) <- count_df$gene_id
for (column_name in setdiff(names(metadata_df), c("sample_id", "bam_file"))) {
  metadata_df[[column_name]] <- as.factor(metadata_df[[column_name]])
}
rownames(metadata_df) <- metadata_df$sample_id

dds <- DESeqDataSetFromMatrix(countData = count_matrix, colData = metadata_df, design = design_formula)
dds <- DESeq(dds)

normalized <- counts(dds, normalized = TRUE)
normalized_df <- data.frame(gene_id = rownames(normalized), normalized, row.names = NULL, check.names = FALSE)
colnames(normalized_df) <- c("gene_id", metadata_df$sample_id)
write_csv_safe(normalized_df, normalized_counts_csv, row.names = FALSE)

result_names <- resultsNames(dds)
available_df <- data.frame(result_name = result_names, stringsAsFactors = FALSE)
write_tsv_safe(available_df, available_results_tsv, row.names = FALSE)

result_name <- trim_scalar(args[["result-name"]] %||% "")
non_intercept <- setdiff(result_names, "Intercept")
if (!nzchar(result_name)) {
  if (length(non_intercept) == 1) {
    result_name <- non_intercept[[1]]
  } else {
    stop("DESeq2 model fit succeeded, but multiple result coefficients are available. Re-run with --result-name '<coef>'.", call. = FALSE)
  }
}
if (!result_name %in% result_names) {
  stop("Requested result-name is not available in the fitted model.", call. = FALSE)
}

res <- results(dds, name = result_name)
res_df <- data.frame(gene_id = rownames(res), as.data.frame(res), row.names = NULL, check.names = FALSE)
write_csv_safe(res_df, deseq_results_csv, row.names = FALSE)

write_lines_safe(c(
  "# DESeq2 Run Report",
  "",
  paste("- Generated:", timestamp_now()),
  paste("- Design formula:", selected_design),
  paste("- Extracted result:", result_name),
  "",
  "## Available Coefficients",
  "",
  markdown_table(available_df)
), run_report_md)
