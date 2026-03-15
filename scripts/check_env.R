`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

script_arg <- commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))][1] %||% ""
script_path <- sub("^--file=", "", script_arg)
script_dir <- normalizePath(dirname(if (nzchar(script_path)) script_path else "scripts/check_env.R"), winslash = "/", mustWork = FALSE)
repo_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)

config_path <- file.path(repo_root, "config.yaml")
config <- if (file.exists(config_path) && requireNamespace("yaml", quietly = TRUE)) yaml::read_yaml(config_path) else list()

required_packages <- c(
  "shiny", "DT", "plotly", "ggplot2", "bslib", "promises", "future",
  "DESeq2", "Rsubread", "AnnotationDbi", "clusterProfiler", "org.Mm.eg.db"
)

package_status <- data.frame(
  package = required_packages,
  installed = vapply(required_packages, requireNamespace, logical(1), quietly = TRUE),
  stringsAsFactors = FALSE
)

samtools_path <- unname(Sys.which("samtools"))[[1]]
if (!nzchar(samtools_path) && !is.null(config$samtools_path)) {
  samtools_path <- tryCatch(normalizePath(config$samtools_path, winslash = "/", mustWork = TRUE), error = function(e) "")
}

results_dir <- file.path(repo_root, "results")
write_ok <- FALSE
if (dir.exists(results_dir)) {
  probe <- file.path(results_dir, paste0(".env_check_", format(Sys.time(), "%Y%m%d%H%M%S"), ".tmp"))
  write_ok <- tryCatch({
    file.create(probe)
    unlink(probe)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

cat("Bulk RNA-seq App environment check\n")
cat("==================================\n")
cat("R version: ", R.version.string, "\n", sep = "")
cat("Repository root: ", repo_root, "\n", sep = "")
cat("config.yaml: ", if (file.exists(config_path)) "found" else "missing", "\n", sep = "")
cat("samtools: ", if (nzchar(samtools_path)) samtools_path else "not found", "\n", sep = "")
cat("results writable: ", if (write_ok) "yes" else "no", "\n", sep = "")
cat("\nR package status:\n")
print(package_status, row.names = FALSE)

if (any(!package_status$installed)) {
  cat("\nMissing dependencies. Run:\n")
  cat("Rscript scripts/install_deps.R\n")
}
