source("scripts/common.R")

config <- load_config("config.yaml")
paths <- resolve_paths(config)
ensure_dirs(planned_result_dirs(paths))

results_csv <- file.path(paths$deseq2_dir, "deseq2_results.csv")
normalized_csv <- file.path(paths$deseq2_dir, "normalized_counts.csv")
annotated_csv <- file.path(paths$annotation_dir, "annotated_deseq2_results.csv")
volcano_png <- file.path(paths$plots_dir, "volcano_plot.png")
volcano_pdf <- file.path(paths$plots_dir, "volcano_plot.pdf")
volcano_labels_csv <- file.path(paths$plots_dir, "volcano_plot_labels.csv")
pca_png <- file.path(paths$qc_dir, "pca_plot.png")
pca_pdf <- file.path(paths$qc_dir, "pca_plot.pdf")
heatmap_png <- file.path(paths$qc_dir, "sample_distance_heatmap.png")
plot_report <- file.path(paths$plots_dir, "plot_report.md")

for (path in c(volcano_png, volcano_pdf, volcano_labels_csv, pca_png, pca_pdf, heatmap_png, plot_report)) {
  assert_no_overwrite(path)
}

if (!file.exists(results_csv) || !file.exists(normalized_csv)) {
  stop("DESeq2 result files are missing. Run scripts/04_deseq2.R first.", call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package ggplot2 is required for plotting.", call. = FALSE)
}

suppressPackageStartupMessages(library(ggplot2))
plot_df <- if (file.exists(annotated_csv)) {
  read.csv(annotated_csv, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  read.csv(results_csv, stringsAsFactors = FALSE, check.names = FALSE)
}
plot_df$neg_log10_padj <- -log10(ifelse(is.na(plot_df$padj) | plot_df$padj <= 0, 1, plot_df$padj))
plot_df$significant <- ifelse(!is.na(plot_df$padj) & plot_df$padj < 0.05 & abs(plot_df$log2FoldChange) >= 1, "yes", "no")
plot_df$label <- if ("SYMBOL" %in% names(plot_df)) {
  ifelse(is.na(plot_df$SYMBOL) | !nzchar(plot_df$SYMBOL), plot_df$gene_id, plot_df$SYMBOL)
} else {
  plot_df$gene_id
}

sig_df <- plot_df[plot_df$significant == "yes", , drop = FALSE]
sig_df <- sig_df[order(sig_df$padj, -abs(sig_df$log2FoldChange), sig_df$label, na.last = TRUE), , drop = FALSE]
top_n <- min(20L, nrow(sig_df))
label_df <- if (top_n > 0) sig_df[seq_len(top_n), c("gene_id", "label", "log2FoldChange", "padj", "neg_log10_padj"), drop = FALSE] else data.frame(
  gene_id = character(0),
  label = character(0),
  log2FoldChange = numeric(0),
  padj = numeric(0),
  neg_log10_padj = numeric(0),
  stringsAsFactors = FALSE
)

volcano <- ggplot(plot_df, aes(x = log2FoldChange, y = neg_log10_padj, color = significant)) +
  geom_point(alpha = 0.7, size = 1.5) +
  scale_color_manual(values = c(yes = "firebrick", no = "grey70")) +
  labs(
    title = "Volcano Plot (480TG vs 270TG)",
    subtitle = "Annotated with top significant genes",
    x = "log2 fold change (480TG vs 270TG)",
    y = "-log10 adjusted p-value"
  ) +
  theme_minimal(base_size = 12)

if (nrow(label_df) > 0) {
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    volcano <- volcano +
      ggrepel::geom_text_repel(
        data = label_df,
        aes(x = log2FoldChange, y = neg_log10_padj, label = label),
        inherit.aes = FALSE,
        size = 3,
        color = "black",
        box.padding = 0.35,
        point.padding = 0.2,
        max.overlaps = Inf,
        show.legend = FALSE
      )
  } else {
    volcano <- volcano +
      geom_text(
        data = label_df,
        aes(x = log2FoldChange, y = neg_log10_padj, label = label),
        inherit.aes = FALSE,
        size = 3,
        color = "black",
        vjust = -0.5,
        check_overlap = TRUE,
        show.legend = FALSE
      )
  }
}

ggsave(volcano_png, volcano, width = 8, height = 6, dpi = 300)
ggsave(volcano_pdf, volcano, width = 8, height = 6)
write_csv_safe(label_df, volcano_labels_csv, row.names = FALSE)

norm_df <- read.csv(normalized_csv, stringsAsFactors = FALSE, check.names = FALSE)
metadata_df <- validate_metadata(read.csv(paths$metadata, stringsAsFactors = FALSE, check.names = FALSE), require_design = TRUE)
sample_columns <- intersect(metadata_df$sample_id, names(norm_df))
metadata_df <- metadata_df[match(sample_columns, metadata_df$sample_id), , drop = FALSE]

norm_matrix <- as.matrix(norm_df[, sample_columns, drop = FALSE])
rownames(norm_matrix) <- norm_df$gene_id
variance <- apply(norm_matrix, 1, var)
selected <- order(variance, decreasing = TRUE)[seq_len(min(500, length(variance)))]
pca <- prcomp(t(log1p(norm_matrix[selected, , drop = FALSE])), scale. = TRUE)
pca_df <- data.frame(
  sample_id = rownames(pca$x),
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  metadata_df[match(rownames(pca$x), metadata_df$sample_id), c("condition", "genotype"), drop = FALSE],
  stringsAsFactors = FALSE
)

pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = condition, shape = genotype)) +
  geom_point(size = 3) +
  labs(title = "Sample PCA", x = "PC1", y = "PC2") +
  theme_minimal(base_size = 12)

ggsave(pca_png, pca_plot, width = 8, height = 6, dpi = 300)
ggsave(pca_pdf, pca_plot, width = 8, height = 6)

heatmap_note <- "Package pheatmap not available; heatmap skipped."
if (requireNamespace("pheatmap", quietly = TRUE)) {
  suppressPackageStartupMessages(library(pheatmap))
  sample_distance <- as.matrix(dist(t(log1p(norm_matrix))))
  annotation_df <- metadata_df[, c("condition", "genotype"), drop = FALSE]
  rownames(annotation_df) <- metadata_df$sample_id
  pheatmap::pheatmap(sample_distance, annotation_col = annotation_df, annotation_row = annotation_df, filename = heatmap_png, width = 8, height = 6)
  heatmap_note <- paste("Heatmap written to", heatmap_png)
}

write_lines_safe(c(
  "# Plot Report",
  "",
  paste("- Generated:", timestamp_now()),
  paste("- Volcano plot:", volcano_png),
  paste("- Volcano labels:", volcano_labels_csv),
  paste("- PCA plot:", pca_png),
  paste("- Heatmap:", heatmap_note)
), plot_report)
