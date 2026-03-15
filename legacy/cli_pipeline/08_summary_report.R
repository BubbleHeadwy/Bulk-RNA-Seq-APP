source("scripts/common.R")

config <- load_config("config.yaml")
paths <- resolve_paths(config)
ensure_dirs(planned_result_dirs(paths))
ensure_dir("reports")

annotated_csv <- file.path(paths$annotation_dir, "annotated_deseq2_results.csv")
go_csv <- file.path(paths$enrichment_dir, "go_enrichment_results.csv")
top_deg_csv <- file.path(paths$deseq2_dir, "top_deg_480TG_vs_270TG.csv")
summary_md <- file.path("reports", "480TG_vs_270TG_summary.md")

assert_no_overwrite(top_deg_csv)
assert_no_overwrite(summary_md)

if (!file.exists(annotated_csv)) {
  stop("Annotated DESeq2 results not found. Run scripts/06_gene_annotation.R first.", call. = FALSE)
}

deg_df <- read.csv(annotated_csv, stringsAsFactors = FALSE, check.names = FALSE)
required_deg_cols <- c("gene_id", "SYMBOL", "GENENAME", "baseMean", "log2FoldChange", "padj")
missing_deg_cols <- setdiff(required_deg_cols, names(deg_df))
if (length(missing_deg_cols) > 0) {
  stop("Annotated DEG table is missing required columns: ", paste(missing_deg_cols, collapse = ", "), call. = FALSE)
}

sig_df <- deg_df[
  !is.na(deg_df$padj) &
    deg_df$padj < 0.05 &
    !is.na(deg_df$log2FoldChange) &
    abs(deg_df$log2FoldChange) >= 1,
  ,
  drop = FALSE
]

sig_df$higher_in <- ifelse(sig_df$log2FoldChange > 0, "480TG", "270TG")
sig_df$display_symbol <- ifelse(is.na(sig_df$SYMBOL) | !nzchar(sig_df$SYMBOL), sig_df$gene_id, sig_df$SYMBOL)

ord <- order(sig_df$padj, -abs(sig_df$log2FoldChange), sig_df$display_symbol, na.last = TRUE)
sig_df <- sig_df[ord, , drop = FALSE]

top_n <- min(20L, nrow(sig_df))
top_deg <- if (top_n > 0) sig_df[seq_len(top_n), c(
  "gene_id", "display_symbol", "GENENAME", "higher_in", "log2FoldChange", "padj", "baseMean", "gtf_gene_biotype"
), drop = FALSE] else data.frame(
  gene_id = character(0),
  display_symbol = character(0),
  GENENAME = character(0),
  higher_in = character(0),
  log2FoldChange = numeric(0),
  padj = numeric(0),
  baseMean = numeric(0),
  gtf_gene_biotype = character(0),
  stringsAsFactors = FALSE
)

names(top_deg) <- c("gene_id", "symbol", "gene_name", "higher_in", "log2FoldChange", "padj", "baseMean", "gene_biotype")
write_csv_safe(top_deg, top_deg_csv, row.names = FALSE)

go_df <- if (file.exists(go_csv)) read.csv(go_csv, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
top_go <- if (nrow(go_df) > 0) go_df[order(go_df$p.adjust, go_df$pvalue), c("ID", "Description", "Count", "p.adjust", "geneID"), drop = FALSE] else data.frame()
top_go <- head(top_go, 10)

tested_genes <- sum(!is.na(deg_df$padj))
sig_count <- nrow(sig_df)
up_count <- sum(sig_df$log2FoldChange > 0, na.rm = TRUE)
down_count <- sum(sig_df$log2FoldChange < 0, na.rm = TRUE)

summary_lines <- c(
  "# 480TG vs 270TG Demo Summary",
  "",
  paste("- Generated:", timestamp_now()),
  "- Comparison: `480TG` vs `270TG`",
  "- Design: `~ genotype`",
  "- Interpretation: positive `log2FoldChange` means higher expression in `480TG`; negative values mean higher expression in `270TG`.",
  "",
  "## DEG Summary",
  "",
  paste("- Genes tested:", tested_genes),
  paste("- Significant genes (`padj < 0.05` and `|log2FC| >= 1`):", sig_count),
  paste("- Higher in 480TG:", up_count),
  paste("- Higher in 270TG:", down_count),
  paste("- Top DEG table:", normalizePath(top_deg_csv, winslash = "/", mustWork = FALSE)),
  "",
  "## Top DEG",
  ""
)

summary_lines <- c(summary_lines, markdown_table(top_deg), "", "## Plots", "")
summary_lines <- c(
  summary_lines,
  paste("- Volcano plot: ", normalizePath(file.path(paths$plots_dir, "volcano_plot.png"), winslash = "/", mustWork = FALSE), sep = ""),
  paste("- PCA plot: ", normalizePath(file.path(paths$qc_dir, "pca_plot.png"), winslash = "/", mustWork = FALSE), sep = ""),
  paste("- Sample distance heatmap: ", normalizePath(file.path(paths$qc_dir, "sample_distance_heatmap.png"), winslash = "/", mustWork = FALSE), sep = ""),
  "",
  "## Top GO Terms",
  ""
)

summary_lines <- c(
  summary_lines,
  if (nrow(top_go) > 0) markdown_table(top_go) else c("- No GO terms available."),
  "",
  "## Output Files",
  "",
  paste("- Annotated DEG table: ", normalizePath(annotated_csv, winslash = "/", mustWork = FALSE), sep = ""),
  paste("- GO enrichment table: ", normalizePath(go_csv, winslash = "/", mustWork = FALSE), sep = ""),
  paste("- DESeq2 run report: ", normalizePath(file.path(paths$deseq2_dir, "deseq2_run_report.md"), winslash = "/", mustWork = FALSE), sep = "")
)

write_lines_safe(summary_lines, summary_md)
