cran_packages <- c(
  "shiny",
  "DT",
  "plotly",
  "ggplot2",
  "bslib",
  "promises",
  "future",
  "htmlwidgets",
  "yaml",
  "pheatmap",
  "ggrepel"
)

bioc_packages <- c(
  "DESeq2",
  "Rsubread",
  "AnnotationDbi",
  "clusterProfiler",
  "org.Mm.eg.db"
)

install_if_missing <- function(packages, installer) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) {
    return(invisible(character(0)))
  }
  message("安装缺失依赖: ", paste(missing, collapse = ", "))
  installer(missing)
  invisible(missing)
}

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos = "https://cloud.r-project.org")
}

install_if_missing(cran_packages, function(pkgs) {
  install.packages(pkgs, repos = "https://cloud.r-project.org")
})

install_if_missing(bioc_packages, function(pkgs) {
  BiocManager::install(pkgs, ask = FALSE, update = FALSE)
})

message("依赖安装检查完成。")
