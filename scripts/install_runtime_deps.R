`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

parse_cli_args <- function(args) {
  parsed <- list()
  if (length(args) == 0) {
    return(parsed)
  }
  i <- 1L
  while (i <= length(args)) {
    current <- args[[i]]
    if (startsWith(current, "--")) {
      key <- sub("^--", "", current)
      if (grepl("=", key, fixed = TRUE)) {
        parsed_key <- sub("=.*$", "", key)
        parsed_value <- sub("^[^=]+=", "", key)
        parsed[[parsed_key]] <- parsed_value
      } else if (i < length(args) && !startsWith(args[[i + 1L]], "--")) {
        parsed[[key]] <- args[[i + 1L]]
        i <- i + 1L
      } else {
        parsed[[key]] <- TRUE
      }
    }
    i <- i + 1L
  }
  parsed
}

args <- parse_cli_args(commandArgs(trailingOnly = TRUE))
target_lib <- normalizePath(args$lib %||% file.path("runtime", "library"), winslash = "/", mustWork = FALSE)
dir.create(target_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(target_lib, .libPaths())))

options(repos = c(CRAN = "https://cloud.r-project.org"))
if (.Platform$OS.type == "windows") {
  options(pkgType = "binary")
}

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

install_cran <- function(pkgs) {
  install.packages(
    pkgs,
    lib = target_lib,
    dependencies = TRUE
  )
}

install_if_missing <- function(packages, installer) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) {
    return(invisible(character(0)))
  }
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  installer(missing)
  invisible(missing)
}

ensure_biocmanager <- function() {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    message("Installing missing package: BiocManager")
    install_cran("BiocManager")
  }
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    stop(
      "Failed to install BiocManager into runtime library: ",
      target_lib,
      "\nPlease check network/CRAN mirror and retry.",
      call. = FALSE
    )
  }
}

install_if_missing(cran_packages, install_cran)
ensure_biocmanager()

install_if_missing(bioc_packages, function(pkgs) {
  BiocManager::install(
    pkgs,
    ask = FALSE,
    update = FALSE,
    lib = target_lib,
    dependencies = TRUE
  )
})

message("Runtime dependency installation finished.")
message("Library path: ", target_lib)
