`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

resolve_script_path <- function() {
  all_args <- commandArgs(trailingOnly = FALSE)
  script_arg <- all_args[grep("^--file=", all_args)][1] %||% ""
  if (!nzchar(script_arg)) {
    return("")
  }
  sub("^--file=", "", script_arg)
}

resolve_app_dir <- function(script_path) {
  if (nzchar(script_path)) {
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE))
  }
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

resolve_runtime_root <- function(app_dir) {
  env_root <- trimws(Sys.getenv("BULKSEQ_RUNTIME_ROOT", unset = ""))
  candidates <- unique(c(env_root, file.path(app_dir, "runtime")))
  for (candidate in candidates) {
    if (nzchar(candidate) && dir.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  ""
}

prepend_runtime_libpath <- function(runtime_root) {
  env_lib <- trimws(Sys.getenv("BULKSEQ_R_LIBS_USER", unset = ""))
  candidates <- unique(c(env_lib, if (nzchar(runtime_root)) file.path(runtime_root, "library") else ""))
  for (candidate in candidates) {
    if (nzchar(candidate) && dir.exists(candidate)) {
      .libPaths(unique(c(normalizePath(candidate, winslash = "/", mustWork = TRUE), .libPaths())))
      break
    }
  }
  invisible(.libPaths())
}

args <- commandArgs(trailingOnly = TRUE)
port <- 3838L
host <- "127.0.0.1"

if (length(args) >= 1) {
  port <- suppressWarnings(as.integer(args[[1]]))
  if (is.na(port)) {
    port <- 3838L
  }
}
if (length(args) >= 2) {
  host <- args[[2]]
}

script_path <- resolve_script_path()
app_dir <- resolve_app_dir(script_path)
runtime_root <- resolve_runtime_root(app_dir)
prepend_runtime_libpath(runtime_root)

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("shiny is not installed. Run: Rscript scripts/install_deps.R", call. = FALSE)
}

setwd(app_dir)
shiny::runApp(".", host = host, port = port, launch.browser = FALSE)
