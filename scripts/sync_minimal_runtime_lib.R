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

clean_dep_token <- function(token) {
  out <- trimws(token)
  out <- sub("\\s*\\(.*\\)$", "", out)
  out <- trimws(out)
  if (!nzchar(out)) {
    return("")
  }
  out
}

parse_dependency_field <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) {
    return(character(0))
  }
  parts <- strsplit(text, ",", fixed = TRUE)[[1]]
  deps <- vapply(parts, clean_dep_token, character(1))
  deps <- deps[nzchar(deps)]
  deps <- setdiff(deps, c("R", "base"))
  unique(deps)
}

copy_package_dir <- function(src, target_root) {
  if (!dir.exists(src)) {
    return(FALSE)
  }
  if (!dir.exists(target_root)) {
    dir.create(target_root, recursive = TRUE, showWarnings = FALSE)
  }
  pkg_name <- basename(src)
  dst <- file.path(target_root, pkg_name)
  if (dir.exists(dst)) {
    unlink(dst, recursive = TRUE, force = TRUE)
  }

  copy_result <- file.copy(src, target_root, recursive = TRUE)
  if (length(copy_result) == 0) {
    return(FALSE)
  }
  isTRUE(all(copy_result)) && dir.exists(dst)
}

args <- parse_cli_args(commandArgs(trailingOnly = TRUE))
source_lib <- normalizePath(args[["source-lib"]] %||% "", winslash = "/", mustWork = FALSE)
target_lib <- normalizePath(args[["target-lib"]] %||% "", winslash = "/", mustWork = FALSE)

if (!nzchar(source_lib) || !dir.exists(source_lib)) {
  stop("source-lib is missing or not a directory.", call. = FALSE)
}
if (!nzchar(target_lib) || !dir.exists(target_lib)) {
  stop("target-lib is missing or not a directory.", call. = FALSE)
}

required_packages <- c(
  "BiocManager",
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
  "ggrepel",
  "DESeq2",
  "Rsubread",
  "AnnotationDbi",
  "clusterProfiler",
  "org.Mm.eg.db"
)

source_meta <- installed.packages(
  lib.loc = source_lib,
  fields = c("Depends", "Imports", "LinkingTo")
)
available <- rownames(source_meta)

missing_required <- setdiff(required_packages, available)
if (length(missing_required) > 0) {
  message("Required packages not found in source library: ", paste(missing_required, collapse = ", "))
}

closure <- character(0)
queue <- intersect(required_packages, available)

while (length(queue) > 0) {
  pkg <- queue[[1]]
  queue <- queue[-1]
  if (pkg %in% closure) {
    next
  }
  closure <- c(closure, pkg)
  dep_fields <- source_meta[pkg, c("Depends", "Imports", "LinkingTo"), drop = TRUE]
  deps <- unique(unlist(lapply(dep_fields, parse_dependency_field), use.names = FALSE))
  deps <- setdiff(deps, closure)
  deps <- deps[deps %in% available]
  if (length(deps) > 0) {
    queue <- unique(c(queue, deps))
  }
}

target_meta <- installed.packages(lib.loc = target_lib, fields = "Priority")
base_keep <- rownames(target_meta)[target_meta[, "Priority"] %in% c("base", "recommended")]
keep_names <- unique(c(base_keep, closure))

existing_dirs <- list.files(target_lib, all.files = FALSE, full.names = FALSE)
copy_list <- setdiff(closure, base_keep)
if (length(copy_list) > 0) {
  message("Copying required packages to target: ", length(copy_list))
  failed <- character(0)
        for (pkg in copy_list) {
            src <- file.path(source_lib, pkg)
            ok <- copy_package_dir(src, target_lib)
            if (!isTRUE(ok)) {
                failed <- c(failed, pkg)
            }
        }
  if (length(failed) > 0) {
    stop("Failed to copy package directories: ", paste(unique(failed), collapse = ", "), call. = FALSE)
  }
}

existing_dirs <- list.files(target_lib, all.files = FALSE, full.names = FALSE)
remove_dirs <- setdiff(existing_dirs, keep_names)
if (length(remove_dirs) > 0) {
  message("Removing non-required packages from target: ", length(remove_dirs))
  invisible(lapply(remove_dirs, function(pkg) {
    unlink(file.path(target_lib, pkg), recursive = TRUE, force = TRUE)
  }))
}

manifest_path <- file.path(dirname(target_lib), "minimal-runtime-packages.txt")
writeLines(sort(unique(copy_list)), con = manifest_path, useBytes = TRUE)

message("Minimal runtime library sync completed.")
message("Target library: ", target_lib)
message("Required package closure size: ", length(closure))
message("Manifest: ", normalizePath(manifest_path, winslash = "/", mustWork = FALSE))
