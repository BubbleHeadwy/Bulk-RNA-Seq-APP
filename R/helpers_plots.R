make_volcano_data <- function(annotated_df, lfc_threshold = 1, padj_threshold = 0.05) {
  df <- annotated_df
  df$log2FoldChange <- suppressWarnings(as.numeric(df$log2FoldChange))
  df$padj <- suppressWarnings(as.numeric(df$padj))
  df$neg_log10_padj <- -log10(ifelse(is.na(df$padj) | df$padj <= 0, 1, df$padj))
  df$display_label <- ifelse(is.na(df$SYMBOL) | !nzchar(df$SYMBOL), df$gene_id, df$SYMBOL)
  is_sig <- !is.na(df$padj) & df$padj <= padj_threshold & !is.na(df$log2FoldChange) & abs(df$log2FoldChange) >= lfc_threshold
  df$regulation <- ifelse(is_sig & df$log2FoldChange > 0, "UP", ifelse(is_sig & df$log2FoldChange < 0, "DOWN", "NO"))
  df
}

top_volcano_labels <- function(df, n = 12) {
  sig_df <- df[df$regulation %in% c("UP", "DOWN"), , drop = FALSE]
  if (nrow(sig_df) == 0) return(sig_df)
  sig_df <- sig_df[order(sig_df$padj, -abs(sig_df$log2FoldChange), sig_df$display_label), , drop = FALSE]
  utils::head(sig_df, n)
}

compute_group_hull <- function(pca_df, group_col) {
  split_df <- split(pca_df, pca_df[[group_col]])
  hulls <- lapply(split_df, function(df) {
    if (nrow(df) < 3) return(NULL)
    idx <- chull(df$PC1, df$PC2)
    df[idx, , drop = FALSE]
  })
  hulls <- hulls[!vapply(hulls, is.null, logical(1))]
  if (length(hulls) == 0) return(data.frame())
  do.call(rbind, hulls)
}

plot_volcano_interactive <- function(annotated_df, lfc_threshold = 1, padj_threshold = 0.05, comparison_label = "") {
  if (!requireNamespace("plotly", quietly = TRUE) || is.null(annotated_df) || nrow(annotated_df) == 0) {
    return(NULL)
  }

  df <- make_volcano_data(annotated_df, lfc_threshold, padj_threshold)
  label_df <- top_volcano_labels(df, n = 12)
  ymax <- max(df$neg_log10_padj, na.rm = TRUE)
  xmin <- min(df$log2FoldChange, na.rm = TRUE)
  xmax <- max(df$log2FoldChange, na.rm = TRUE)
  stats_df <- as.list(table(factor(df$regulation, levels = c("UP", "DOWN", "NO"))))

  p <- plotly::plot_ly(
    df,
    x = ~log2FoldChange,
    y = ~neg_log10_padj,
    color = ~regulation,
    colors = c("UP" = "#ef4444", "DOWN" = "#3b82f6", "NO" = "#b0b7c3"),
    type = "scatter",
    mode = "markers",
    showlegend = FALSE,
    text = ~paste0(
      "基因: ", display_label,
      "<br>gene_id: ", gene_id,
      "<br>log2FC: ", round(log2FoldChange, 3),
      "<br>padj: ", signif(padj, 3),
      "<br>分类: ", regulation
    ),
    hoverinfo = "text"
  )

  if (nrow(label_df) > 0) {
    p <- plotly::add_trace(
      p,
      data = label_df,
      x = ~log2FoldChange,
      y = ~neg_log10_padj,
      type = "scatter",
      mode = "markers+text",
      text = ~display_label,
      textposition = "top center",
      textfont = list(color = "#7f1d1d", size = 12),
      marker = list(color = "#7f1d1d", size = 8, symbol = "diamond"),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    )
  }

  p |>
    plotly::layout(
      title = if (nzchar(comparison_label)) comparison_label else "火山图",
      xaxis = list(title = "log2 fold change"),
      yaxis = list(title = "-log10 adjusted p-value"),
      showlegend = FALSE,
      shapes = list(
        list(type = "line", x0 = -lfc_threshold, x1 = -lfc_threshold, y0 = 0, y1 = ymax, line = list(color = "#64748b", dash = "dash")),
        list(type = "line", x0 = lfc_threshold, x1 = lfc_threshold, y0 = 0, y1 = ymax, line = list(color = "#64748b", dash = "dash")),
        list(type = "line", x0 = xmin, x1 = xmax, y0 = -log10(padj_threshold), y1 = -log10(padj_threshold), line = list(color = "#64748b", dash = "dash"))
      ),
      annotations = list(
        list(x = 0.02, y = 0.98, xref = "paper", yref = "paper", text = paste0("pvalue<=", padj_threshold, "<br>|log2FoldChange|>=", lfc_threshold), showarrow = FALSE, align = "left", xanchor = "left", yanchor = "top", font = list(size = 12, color = "#111827")),
        list(x = 0.02, y = 0.915, xref = "paper", yref = "paper", text = paste0("UP ", stats_df$UP), showarrow = FALSE, align = "left", xanchor = "left", yanchor = "top", font = list(size = 12, color = "#ef4444")),
        list(x = 0.02, y = 0.87, xref = "paper", yref = "paper", text = paste0("DOWN ", stats_df$DOWN), showarrow = FALSE, align = "left", xanchor = "left", yanchor = "top", font = list(size = 12, color = "#3b82f6")),
        list(x = 0.02, y = 0.825, xref = "paper", yref = "paper", text = paste0("NO ", stats_df$NO), showarrow = FALSE, align = "left", xanchor = "left", yanchor = "top", font = list(size = 12, color = "#9ca3af"))
      )
    )
}

plot_pca_interactive <- function(pca_df, color_var = "condition", shape_var = "genotype", title = "PCA 图") {
  if (!requireNamespace("plotly", quietly = TRUE) || !requireNamespace("ggplot2", quietly = TRUE) || nrow(pca_df) == 0) {
    return(NULL)
  }

  percent_var <- attr(pca_df, "percent_var")
  pc1_label <- if (!is.null(percent_var) && length(percent_var) >= 1) sprintf("PC1 (%.2f%%)", percent_var[[1]] * 100) else "PC1"
  pc2_label <- if (!is.null(percent_var) && length(percent_var) >= 2) sprintf("PC2 (%.2f%%)", percent_var[[2]] * 100) else "PC2"
  color_var <- if (color_var %in% names(pca_df)) color_var else setdiff(names(pca_df), c("sample_id", "PC1", "PC2"))[[1]]
  shape_var <- if (shape_var %in% names(pca_df)) shape_var else color_var
  pca_df$.group <- as.factor(pca_df[[color_var]])
  pca_df$.shape <- as.factor(pca_df[[shape_var]])
  pca_df$.tooltip <- paste0(
    "样本: ", pca_df$sample_id,
    "<br>", color_var, ": ", pca_df[[color_var]],
    paste0("<br>", shape_var, ": ", pca_df[[shape_var]]),
    "<br>PC1: ", round(pca_df$PC1, 3),
    "<br>PC2: ", round(pca_df$PC2, 3)
  )
  hull_df <- compute_group_hull(pca_df, ".group")

  p <- ggplot2::ggplot() +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#b7b7b7", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#b7b7b7", linewidth = 0.8) +
    ggplot2::labs(title = title, x = pc1_label, y = pc2_label, color = color_var, fill = color_var) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "#dddddd"),
      panel.grid.minor = ggplot2::element_line(color = "#eeeeee"),
      axis.title = ggplot2::element_text(size = 18),
      axis.text = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 18, face = "bold"),
      legend.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14)
    )

  if (nrow(hull_df) > 0) {
    p <- p + ggplot2::geom_polygon(
      data = hull_df,
      ggplot2::aes(x = PC1, y = PC2, group = .group, fill = .group),
      alpha = 0.18,
      color = NA,
      show.legend = FALSE
    )
  }

  p <- p +
    ggplot2::geom_point(data = pca_df, ggplot2::aes(x = PC1, y = PC2, color = .group, shape = .shape, text = .tooltip), size = 4) +
    ggrepel::geom_text_repel(
      data = pca_df,
      ggplot2::aes(x = PC1, y = PC2, label = sample_id, color = .group),
      size = 5,
      show.legend = FALSE,
      box.padding = 0.35,
      point.padding = 0.2,
      segment.color = NA,
      max.overlaps = Inf
    )

  p <- p + ggplot2::labs(shape = shape_var)
  plotly::ggplotly(p, tooltip = "text")
}

plot_heatmap_interactive <- function(normalized_df, metadata_df, annotated_df = NULL, top_n = 50, title = "热图") {
  if (!requireNamespace("plotly", quietly = TRUE) || is.null(normalized_df) || nrow(normalized_df) == 0) {
    return(NULL)
  }
  sample_columns <- intersect(metadata_df$sample_id, names(normalized_df))
  if (length(sample_columns) < 2) {
    return(NULL)
  }

  mat <- as.matrix(normalized_df[, sample_columns, drop = FALSE])
  rownames(mat) <- normalized_df$gene_id
  vars <- apply(mat, 1, stats::var, na.rm = TRUE)
  vars[is.na(vars)] <- 0
  selected <- order(vars, decreasing = TRUE)[seq_len(min(top_n, length(vars)))]
  heatmap_mat <- log1p(mat[selected, , drop = FALSE])

  gene_ids <- rownames(heatmap_mat)
  gene_labels <- gene_ids
  if (!is.null(annotated_df) && nrow(annotated_df) > 0 && all(c("gene_id", "SYMBOL") %in% names(annotated_df))) {
    idx <- match(gene_ids, annotated_df$gene_id)
    symbol <- annotated_df$SYMBOL[idx]
    gene_labels <- ifelse(is.na(symbol) | !nzchar(symbol), gene_ids, paste0(symbol, " (", gene_ids, ")"))
  }

  sample_meta <- metadata_df[match(sample_columns, metadata_df$sample_id), , drop = FALSE]
  sample_labels <- sample_columns
  if (all(c("condition", "genotype") %in% names(sample_meta))) {
    sample_labels <- paste0(sample_columns, "<br>", sample_meta$genotype, " / ", sample_meta$condition)
  }

  hover_text <- outer(
    gene_labels,
    sample_columns,
    Vectorize(function(gene_label, sample_id) {
      value <- heatmap_mat[which(gene_labels == gene_label)[1], which(sample_columns == sample_id)[1]]
      paste0("基因: ", gene_label, "<br>样本: ", sample_id, "<br>表达值(log1p): ", round(value, 3))
    })
  )

  plotly::plot_ly(
    x = sample_labels,
    y = gene_labels,
    z = heatmap_mat,
    text = hover_text,
    hoverinfo = "text",
    type = "heatmap",
    colorscale = "RdBu"
  ) |>
    plotly::layout(
      title = title,
      xaxis = list(title = "样本"),
      yaxis = list(title = "基因注释")
    )
}

plot_go_interactive <- function(go_df) {
  if (!requireNamespace("plotly", quietly = TRUE) || is.null(go_df) || nrow(go_df) == 0) {
    return(NULL)
  }
  top_go <- go_df
  top_go$Description <- as.character(top_go$Description)
  top_go$Count <- suppressWarnings(as.numeric(as.character(top_go$Count)))
  top_go$p.adjust <- suppressWarnings(as.numeric(as.character(top_go$p.adjust)))
  top_go <- top_go[!is.na(top_go$Count) & !is.na(top_go$p.adjust) & nzchar(top_go$Description), , drop = FALSE]
  if (nrow(top_go) == 0) {
    return(NULL)
  }
  top_go <- top_go[order(top_go$p.adjust, decreasing = FALSE), , drop = FALSE]
  top_go <- utils::head(top_go, 15)
  top_go <- top_go[nrow(top_go):1, , drop = FALSE]

  plotly::plot_ly(
    top_go,
    x = ~Count,
    y = ~Description,
    type = "bar",
    orientation = "h",
    marker = list(
      color = top_go$p.adjust,
      colorscale = "Reds",
      colorbar = list(title = "p.adjust")
    ),
    text = ~paste0("通路: ", Description, "<br>Count: ", Count, "<br>p.adjust: ", signif(p.adjust, 3)),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = "",
      xaxis = list(title = "基因数"),
      yaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = top_go$Description)
    )
}

save_volcano_png <- function(annotated_df, settings, file) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("缺少 ggplot2，无法导出火山图。", call. = FALSE)
  df <- make_volcano_data(annotated_df, settings$lfc_threshold, settings$padj_threshold)
  label_df <- top_volcano_labels(df, n = 12)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = log2FoldChange, y = neg_log10_padj, color = regulation)) +
    ggplot2::geom_point(alpha = 0.8, size = 1.8) +
    ggplot2::scale_color_manual(values = c("UP" = "#ef4444", "DOWN" = "#3b82f6", "NO" = "#b0b7c3")) +
    ggplot2::geom_vline(xintercept = c(-settings$lfc_threshold, settings$lfc_threshold), linetype = "dashed", color = "#64748b") +
    ggplot2::geom_hline(yintercept = -log10(settings$padj_threshold), linetype = "dashed", color = "#64748b") +
    ggplot2::labs(
      title = paste0(settings$experiment_group, " vs ", settings$control_group),
      x = "log2 fold change",
      y = "-log10 adjusted p-value",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)
  if (nrow(label_df) > 0) {
    p <- p + ggplot2::geom_text(data = label_df, ggplot2::aes(label = display_label), vjust = -0.6, size = 3, show.legend = FALSE, check_overlap = TRUE)
  }
  ggplot2::ggsave(file, p, width = 10, height = 7, dpi = 300)
}

save_pca_png <- function(analysis, file, title = "PCA 图", color_var = NULL, shape_var = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("ggrepel", quietly = TRUE)) stop("缺少 ggplot2 或 ggrepel，无法导出 PCA 图。", call. = FALSE)
  pca_df <- analysis$pca
  if (is.null(pca_df) || nrow(pca_df) == 0) stop("当前没有可导出的 PCA 数据。", call. = FALSE)
  vars <- setdiff(names(pca_df), c("sample_id", "PC1", "PC2"))
  color_var <- if (!is.null(color_var) && color_var %in% vars) color_var else if ("condition" %in% vars) "condition" else vars[[1]]
  shape_var <- if (!is.null(shape_var) && shape_var %in% vars) shape_var else if ("genotype" %in% vars) "genotype" else color_var
  pca_df$.group <- as.factor(pca_df[[color_var]])
  pca_df$.shape <- as.factor(pca_df[[shape_var]])
  hull_df <- compute_group_hull(pca_df, ".group")
  percent_var <- attr(pca_df, "percent_var")
  pc1_label <- if (!is.null(percent_var) && length(percent_var) >= 1) sprintf("PC1 (%.2f%%)", percent_var[[1]] * 100) else "PC1"
  pc2_label <- if (!is.null(percent_var) && length(percent_var) >= 2) sprintf("PC2 (%.2f%%)", percent_var[[2]] * 100) else "PC2"

  p <- ggplot2::ggplot() +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#b7b7b7", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#b7b7b7", linewidth = 0.8)
  if (nrow(hull_df) > 0) {
    p <- p + ggplot2::geom_polygon(data = hull_df, ggplot2::aes(x = PC1, y = PC2, group = .group, fill = .group), alpha = 0.18, color = NA, show.legend = FALSE)
  }
  p <- p +
    ggplot2::geom_point(data = pca_df, ggplot2::aes(x = PC1, y = PC2, color = .group, shape = .shape), size = 4) +
    ggrepel::geom_text_repel(data = pca_df, ggplot2::aes(x = PC1, y = PC2, label = sample_id, color = .group), size = 5, show.legend = FALSE, box.padding = 0.35, point.padding = 0.25, segment.color = NA, max.overlaps = Inf) +
    ggplot2::labs(title = title, x = pc1_label, y = pc2_label, color = color_var, fill = color_var, shape = shape_var)
  ggplot2::ggsave(file, p, width = 10, height = 8, dpi = 300)
}

save_heatmap_png <- function(analysis, top_n, file) {
  normalized_df <- analysis$normalized
  metadata_df <- analysis$metadata
  annotated_df <- analysis$annotated
  if (is.null(normalized_df) || is.null(metadata_df) || nrow(normalized_df) == 0) stop("当前没有可导出的热图数据。", call. = FALSE)
  sample_columns <- intersect(metadata_df$sample_id, names(normalized_df))
  mat <- as.matrix(normalized_df[, sample_columns, drop = FALSE])
  rownames(mat) <- normalized_df$gene_id
  vars <- apply(mat, 1, stats::var, na.rm = TRUE)
  vars[is.na(vars)] <- 0
  selected <- order(vars, decreasing = TRUE)[seq_len(min(top_n, length(vars)))]
  heatmap_mat <- log1p(mat[selected, , drop = FALSE])
  if (!is.null(annotated_df) && nrow(annotated_df) > 0 && all(c("gene_id", "SYMBOL") %in% names(annotated_df))) {
    idx <- match(rownames(heatmap_mat), annotated_df$gene_id)
    symbol <- annotated_df$SYMBOL[idx]
    rownames(heatmap_mat) <- ifelse(is.na(symbol) | !nzchar(symbol), rownames(heatmap_mat), paste0(symbol, " (", rownames(heatmap_mat), ")"))
  }
  title <- if (!is.null(analysis$comparison_label) && nzchar(analysis$comparison_label)) analysis$comparison_label else "热图"
  if (requireNamespace("pheatmap", quietly = TRUE)) {
    annotation_df <- metadata_df[, intersect(c("condition", "genotype"), names(metadata_df)), drop = FALSE]
    rownames(annotation_df) <- metadata_df$sample_id
    pheatmap::pheatmap(heatmap_mat, annotation_col = annotation_df, filename = file, width = 10, height = 12, main = title)
  } else {
    grDevices::png(file, width = 1200, height = 1400, res = 180)
    op <- graphics::par(mar = c(8, 14, 4, 2))
    on.exit({ graphics::par(op); grDevices::dev.off() }, add = TRUE)
    graphics::image(t(heatmap_mat[nrow(heatmap_mat):1, , drop = FALSE]), axes = FALSE, col = grDevices::colorRampPalette(c("#1d4ed8", "#f8fafc", "#b91c1c"))(100), main = title)
    graphics::axis(1, at = seq(0, 1, length.out = ncol(heatmap_mat)), labels = colnames(heatmap_mat), las = 2, cex.axis = 0.8)
    graphics::axis(2, at = seq(0, 1, length.out = nrow(heatmap_mat)), labels = rev(rownames(heatmap_mat)), las = 2, cex.axis = 0.5)
  }
}

save_go_png <- function(go_df, file) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("缺少 ggplot2，无法导出 GO 图。", call. = FALSE)
  top_go <- go_df
  top_go$Description <- as.character(top_go$Description)
  top_go$Count <- suppressWarnings(as.numeric(as.character(top_go$Count)))
  top_go$p.adjust <- suppressWarnings(as.numeric(as.character(top_go$p.adjust)))
  top_go <- top_go[!is.na(top_go$Count) & !is.na(top_go$p.adjust) & nzchar(top_go$Description), , drop = FALSE]
  if (nrow(top_go) == 0) stop("当前没有可导出的 GO 富集结果。", call. = FALSE)
  top_go <- utils::head(top_go[order(top_go$p.adjust), , drop = FALSE], 15)
  top_go$Description <- factor(top_go$Description, levels = rev(top_go$Description))
  p <- ggplot2::ggplot(top_go, ggplot2::aes(x = Count, y = Description, fill = p.adjust)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_gradient(low = "#fee2e2", high = "#b91c1c") +
    ggplot2::labs(title = "", x = "基因数", y = "", fill = "p.adjust") +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(file, p, width = 11, height = 7.5, dpi = 300)
}
