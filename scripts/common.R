resolve_common_file <- function() {
  candidates <- c(
    file.path(getwd(), "R", "common.R"),
    file.path(getwd(), "..", "R", "common.R")
  )
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Cannot locate R/common.R from scripts/common.R", call. = FALSE)
}

source(resolve_common_file(), local = FALSE)
