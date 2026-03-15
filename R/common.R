bootstrap_library_path <- function(config_path = "config.yaml") {
  if (!file.exists(config_path)) {
    return(invisible(NULL))
  }
  lines <- readLines(config_path, warn = FALSE, encoding = "UTF-8")
  match <- grep("^\\s*r_lib_path\\s*:", lines, value = TRUE)
  if (length(match) == 0) {
    return(invisible(NULL))
  }
  value <- sub("^[^:]+:\\s*", "", match[[1]])
  value <- gsub("^['\"]|['\"]$", "", trimws(value))
  if (nzchar(value) && dir.exists(value)) {
    .libPaths(unique(c(normalizePath(value, winslash = "/", mustWork = FALSE), .libPaths())))
  }
  invisible(NULL)
}

bootstrap_library_path()

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required to run this project.", call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

trim_scalar <- function(x) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) {
    return("")
  }
  trimws(as.character(x[[1]]))
}

project_root <- function() {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

load_config <- function(path = "config.yaml") {
  yaml::read_yaml(path)
}

apply_config_libpath <- function(config) {
  lib_path <- trim_scalar(config$r_lib_path %||% "")
  if (nzchar(lib_path) && dir.exists(lib_path)) {
    .libPaths(unique(c(normalizePath(lib_path, winslash = "/", mustWork = FALSE), .libPaths())))
  }
  invisible(.libPaths())
}

is_absolute_path <- function(path) {
  grepl("^[A-Za-z]:[/\\\\]", path) || startsWith(path, "/")
}

resolve_path <- function(root, path) {
  if (!nzchar(trim_scalar(path))) {
    return("")
  }
  candidate <- if (is_absolute_path(path)) path else file.path(root, path)
  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

resolve_paths <- function(config) {
  apply_config_libpath(config)
  repo_root <- project_root()
  output_dir <- resolve_path(repo_root, config$output_dir %||% "results")
  list(
    repo_root = repo_root,
    raw_data_dir = resolve_path(repo_root, config$raw_data_dir),
    bam_dir = resolve_path(config$raw_data_dir, config$bam_dir),
    reference_dir = resolve_path(config$raw_data_dir, config$reference_dir),
    gtf_file = resolve_path(config$raw_data_dir, config$gtf_file),
    genome_fasta = resolve_path(config$raw_data_dir, config$genome_fasta),
    gene_fasta = resolve_path(config$raw_data_dir, config$gene_fasta),
    metadata = resolve_path(repo_root, config$metadata),
    metadata_dir = dirname(resolve_path(repo_root, config$metadata)),
    output_dir = output_dir,
    inventory_dir = file.path(output_dir, "01_inventory"),
    counts_dir = file.path(output_dir, "02_counts"),
    deseq2_dir = file.path(output_dir, "03_deseq2"),
    qc_dir = file.path(output_dir, "04_qc"),
    plots_dir = file.path(output_dir, "05_plots"),
    annotation_dir = file.path(output_dir, "06_annotation"),
    enrichment_dir = file.path(output_dir, "07_enrichment"),
    logs_dir = file.path(output_dir, "08_logs"),
    tmp_dir = file.path(output_dir, "09_tmp")
  )
}

planned_result_dirs <- function(paths) {
  c(
    paths$output_dir,
    paths$inventory_dir,
    paths$qc_dir,
    paths$counts_dir,
    paths$deseq2_dir,
    paths$plots_dir,
    paths$annotation_dir,
    paths$enrichment_dir,
    paths$logs_dir,
    paths$tmp_dir
  )
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

ensure_dirs <- function(paths) {
  invisible(lapply(paths, ensure_dir))
}

assert_no_overwrite <- function(path) {
  if (file.exists(path)) {
    stop("Refusing to overwrite existing output: ", path, call. = FALSE)
  }
}

write_lines_safe <- function(lines, path) {
  assert_no_overwrite(path)
  writeLines(enc2utf8(lines), con = path, useBytes = TRUE)
}

write_csv_safe <- function(data, path, row.names = FALSE) {
  assert_no_overwrite(path)
  write.csv(data, file = path, row.names = row.names, quote = TRUE, na = "")
}

write_tsv_safe <- function(data, path, row.names = FALSE) {
  assert_no_overwrite(path)
  write.table(data, file = path, sep = "\t", quote = FALSE, row.names = row.names, col.names = TRUE, na = "")
}

timestamp_now <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
}

configured_command_path <- function(config, field_name) {
  configured <- trim_scalar(config[[field_name]] %||% "")
  if (!nzchar(configured)) {
    return("")
  }
  tryCatch(
    normalizePath(configured, winslash = "/", mustWork = TRUE),
    error = function(e) ""
  )
}

command_path <- function(command, config = NULL, config_field = NULL) {
  if (!is.null(config) && !is.null(config_field)) {
    configured <- configured_command_path(config, config_field)
    if (nzchar(configured)) {
      return(configured)
    }
  }
  path <- Sys.which(command)[[1]]
  if (is.null(path)) "" else unname(path)
}

collect_command_status <- function(commands, config = NULL, config_fields = list()) {
  resolved_paths <- vapply(commands, function(cmd) {
    field_name <- config_fields[[cmd]] %||% NULL
    command_path(cmd, config = config, config_field = field_name)
  }, character(1))
  data.frame(
    command = commands,
    path = resolved_paths,
    available = nzchar(resolved_paths),
    stringsAsFactors = FALSE
  )
}

collect_package_status <- function(packages) {
  data.frame(
    package = packages,
    installed = vapply(packages, requireNamespace, logical(1), quietly = TRUE),
    version = vapply(packages, function(pkg) if (requireNamespace(pkg, quietly = TRUE)) as.character(packageVersion(pkg)) else NA_character_, character(1)),
    libpath = vapply(packages, function(pkg) if (requireNamespace(pkg, quietly = TRUE)) find.package(pkg)[1] else NA_character_, character(1)),
    stringsAsFactors = FALSE
  )
}

path_status <- function(named_paths) {
  data.frame(
    resource = names(named_paths),
    path = unname(vapply(named_paths, identity, character(1))),
    exists = file.exists(unname(vapply(named_paths, identity, character(1)))),
    stringsAsFactors = FALSE
  )
}

glob_match_any <- function(values, patterns) {
  if (length(patterns) == 0) {
    return(rep(FALSE, length(values)))
  }
  Reduce(`|`, lapply(patterns, function(pattern) grepl(glob2rx(pattern), values)))
}

scan_bam_inventory <- function(paths, config) {
  files <- list.files(paths$bam_dir, full.names = TRUE, recursive = FALSE, no.. = TRUE)
  if (length(files) == 0) {
    file_records <- data.frame(file_name = character(0), file_path = character(0), file_type = character(0), ignored = logical(0), stringsAsFactors = FALSE)
    candidate_records <- data.frame(sample_id = character(0), bam_file = character(0), bam_path = character(0), bai_file = character(0), bai_path = character(0), bai_exists = logical(0), stringsAsFactors = FALSE)
    return(list(files = file_records, candidates = candidate_records, missing_bai = candidate_records))
  }

  basenames <- basename(files)
  is_bad <- grepl("\\.BAD$", basenames, ignore.case = TRUE)
  is_bam <- grepl("\\.bam$", basenames, ignore.case = TRUE)
  is_bai <- grepl("\\.bam\\.bai$", basenames, ignore.case = TRUE)
  ignored_by_pattern <- glob_match_any(basenames, config$ignore_patterns %||% character(0))

  file_records <- data.frame(
    file_name = basenames,
    file_path = normalizePath(files, winslash = "/", mustWork = FALSE),
    file_type = ifelse(is_bam, "bam", ifelse(is_bai, "bam_bai", ifelse(is_bad, "ignored_bad", "other"))),
    ignored = ignored_by_pattern | is_bad,
    stringsAsFactors = FALSE
  )

  bam_records <- file_records[file_records$file_type == "bam" & !file_records$ignored, , drop = FALSE]
  if (nrow(bam_records) == 0) {
    candidate_records <- data.frame(sample_id = character(0), bam_file = character(0), bam_path = character(0), bai_file = character(0), bai_path = character(0), bai_exists = logical(0), stringsAsFactors = FALSE)
  } else {
    candidate_records <- data.frame(
      sample_id = tools::file_path_sans_ext(bam_records$file_name),
      bam_file = bam_records$file_name,
      bam_path = bam_records$file_path,
      bai_file = paste0(bam_records$file_name, ".bai"),
      bai_path = paste0(bam_records$file_path, ".bai"),
      stringsAsFactors = FALSE
    )
    candidate_records$bai_exists <- file.exists(candidate_records$bai_path)
  }

  list(files = file_records, candidates = candidate_records, missing_bai = candidate_records[!candidate_records$bai_exists, , drop = FALSE])
}

run_command <- function(command, args = character(0)) {
  safe_args <- if (length(args) == 0) character(0) else vapply(args, shQuote, character(1), type = "cmd")
  output <- tryCatch(system2(command, args = safe_args, stdout = TRUE, stderr = TRUE), error = function(e) structure(conditionMessage(e), status = 1L))
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  list(status = as.integer(status), output = as.character(output))
}

run_quickcheck <- function(candidates, samtools_path) {
  if (nrow(candidates) == 0) {
    return(data.frame(sample_id = character(0), bam_file = character(0), quickcheck_status = character(0), quickcheck_message = character(0), stringsAsFactors = FALSE))
  }
  if (!nzchar(samtools_path)) {
    return(data.frame(sample_id = candidates$sample_id, bam_file = candidates$bam_file, quickcheck_status = rep("not_available", nrow(candidates)), quickcheck_message = rep("samtools not available; quickcheck not run.", nrow(candidates)), stringsAsFactors = FALSE))
  }

  do.call(rbind, lapply(seq_len(nrow(candidates)), function(i) {
    sample <- candidates[i, , drop = FALSE]
    result <- run_command(samtools_path, c("quickcheck", sample$bam_path))
    data.frame(sample_id = sample$sample_id, bam_file = sample$bam_file, quickcheck_status = if (result$status == 0L) "pass" else "fail", quickcheck_message = if (length(result$output) > 0) paste(result$output, collapse = " ") else if (result$status == 0L) "quickcheck passed." else "quickcheck failed.", stringsAsFactors = FALSE)
  }))
}

generate_sample_sheet_template <- function(candidates) {
  data.frame(sample_id = candidates$sample_id, bam_file = candidates$bam_file, condition = "", genotype = "", replicate = "", stringsAsFactors = FALSE)
}

validate_metadata <- function(metadata_df, inventory_df = NULL, require_design = FALSE) {
  required_columns <- c("sample_id", "bam_file", "condition", "genotype", "replicate")
  missing_columns <- setdiff(required_columns, names(metadata_df))
  if (length(missing_columns) > 0) {
    stop("Metadata missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  metadata_df[] <- lapply(metadata_df, function(column) {
    if (is.character(column)) {
      trimmed <- trimws(column)
      trimmed[is.na(trimmed)] <- ""
      trimmed
    } else {
      column
    }
  })
  if (nrow(metadata_df) == 0) {
    stop("Metadata is empty.", call. = FALSE)
  }
  if (any(!nzchar(metadata_df$sample_id))) stop("Metadata contains empty sample_id values.", call. = FALSE)
  if (any(!nzchar(metadata_df$bam_file))) stop("Metadata contains empty bam_file values.", call. = FALSE)
  if (anyDuplicated(metadata_df$sample_id)) stop("Metadata contains duplicated sample_id values.", call. = FALSE)
  if (anyDuplicated(metadata_df$bam_file)) stop("Metadata contains duplicated bam_file values.", call. = FALSE)
  if (!is.null(inventory_df) && nrow(inventory_df) > 0) {
    missing_bam <- setdiff(metadata_df$bam_file, inventory_df$bam_file)
    if (length(missing_bam) > 0) {
      stop("Metadata references BAM files that are not in the inventory candidate set: ", paste(missing_bam, collapse = ", "), call. = FALSE)
    }
  }
  if (require_design) {
    for (column_name in c("condition", "genotype", "replicate")) {
      if (any(!nzchar(metadata_df[[column_name]]))) {
        stop("Metadata has empty values in required design column: ", column_name, call. = FALSE)
      }
    }
  }
  metadata_df
}

metadata_design_complete <- function(metadata_df) {
  all(vapply(c("condition", "genotype", "replicate"), function(column_name) {
    values <- trimws(as.character(metadata_df[[column_name]]))
    values[is.na(values)] <- ""
    all(nzchar(values))
  }, logical(1)))
}

generate_design_candidates <- function(metadata_df) {
  candidate_columns <- setdiff(names(metadata_df), c("sample_id", "bam_file"))
  candidate_columns <- candidate_columns[vapply(candidate_columns, function(column_name) {
    values <- trimws(as.character(metadata_df[[column_name]]))
    values[is.na(values)] <- ""
    all(nzchar(values)) && length(unique(values)) > 1
  }, logical(1))]
  if (length(candidate_columns) == 0) {
    return(data.frame(formula = character(0), stringsAsFactors = FALSE))
  }
  formulas <- paste("~", candidate_columns)
  if (length(candidate_columns) > 1) {
    formulas <- c(formulas, paste("~", paste(candidate_columns, collapse = " + ")))
  }
  formulas <- unique(formulas)
  data.frame(formula = formulas, stringsAsFactors = FALSE)
}

parse_cli_args <- function(args) {
  parsed <- list()
  if (length(args) == 0) return(parsed)
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

select_count_engine <- function(config) {
  featurecounts_path <- command_path("featureCounts", config = config, config_field = "featurecounts_path")
  if (tolower(trim_scalar(config$count_method %||% "featureCounts")) == "featurecounts" && nzchar(featurecounts_path)) {
    return(list(engine = "featureCounts", command = featurecounts_path))
  }
  if (requireNamespace("Rsubread", quietly = TRUE)) {
    return(list(engine = "Rsubread", command = "Rsubread::featureCounts"))
  }
  if (nzchar(featurecounts_path)) {
    return(list(engine = "featureCounts", command = featurecounts_path))
  }
  list(engine = NA_character_, command = "")
}

prepare_annotation_for_counting <- function(gtf_path, engine, tmp_dir) {
  if (!grepl("\\.gz$", gtf_path, ignore.case = TRUE) || engine != "Rsubread") {
    return(gtf_path)
  }
  ensure_dir(tmp_dir)
  out_path <- file.path(tmp_dir, sub("\\.gz$", "", basename(gtf_path), ignore.case = TRUE))
  if (file.exists(out_path)) return(normalizePath(out_path, winslash = "/", mustWork = TRUE))
  input <- gzfile(gtf_path, open = "rb")
  output <- file(out_path, open = "wb")
  on.exit({ close(input); close(output) }, add = TRUE)
  repeat {
    chunk <- readBin(input, what = raw(), n = 1024 * 1024)
    if (length(chunk) == 0) break
    writeBin(chunk, output)
  }
  normalizePath(out_path, winslash = "/", mustWork = TRUE)
}

infer_pairing_status <- function(bam_path, samtools_path) {
  if (!nzchar(samtools_path)) {
    return(list(status = "unknown", ratio = NA_real_, reason = "samtools not available"))
  }
  total <- run_command(samtools_path, c("view", "-c", "-F", "260", bam_path))
  paired <- run_command(samtools_path, c("view", "-c", "-f", "1", bam_path))
  total_count <- suppressWarnings(as.numeric(trim_scalar(tail(total$output, 1))))
  paired_count <- suppressWarnings(as.numeric(trim_scalar(tail(paired$output, 1))))
  if (total$status != 0L || paired$status != 0L || is.na(total_count) || is.na(paired_count) || total_count <= 0) {
    return(list(status = "unknown", ratio = NA_real_, reason = "Unable to determine pairing state from BAM"))
  }
  ratio <- paired_count / total_count
  if (paired_count == 0) return(list(status = "single_end", ratio = ratio, reason = "No paired reads detected"))
  if (ratio >= 0.8) return(list(status = "paired_end", ratio = ratio, reason = sprintf("Paired-read fraction %.3f", ratio)))
  list(status = "mixed", ratio = ratio, reason = sprintf("Mixed pairing state detected (fraction %.3f)", ratio))
}

infer_strandedness <- function(config, paths, bam_paths) {
  infer_tool <- command_path("infer_experiment.py", config = config, config_field = "infer_experiment_path")
  bed_path <- trim_scalar(config$rseqc_bed %||% config$strandedness_bed %||% "")
  if (!nzchar(infer_tool) || !nzchar(bed_path)) {
    return(list(status = "unknown", strand_specific = 0L, note = "Automatic strandedness detection unavailable; defaulting to unstranded. Please confirm manually."))
  }
  resolved_bed <- resolve_path(paths$repo_root, bed_path)
  if (!file.exists(resolved_bed) || length(bam_paths) == 0) {
    return(list(status = "unknown", strand_specific = 0L, note = "Strandedness reference BED not available; defaulting to unstranded. Please confirm manually."))
  }
  result <- run_command(infer_tool, c("-r", resolved_bed, "-i", bam_paths[[1]]))
  if (result$status != 0L) {
    return(list(status = "unknown", strand_specific = 0L, note = "infer_experiment.py failed; defaulting to unstranded. Please confirm manually."))
  }
  text <- paste(result$output, collapse = "\n")
  forward_match <- regmatches(text, regexpr("[0-9.]+(?=%.*1\\+\\+,1--,2\\+-,2-\\+)", text, perl = TRUE))
  reverse_match <- regmatches(text, regexpr("[0-9.]+(?=%.*1\\+-,1-\\+,2\\+\\+,2--)", text, perl = TRUE))
  forward <- suppressWarnings(as.numeric(forward_match))
  reverse <- suppressWarnings(as.numeric(reverse_match))
  if (!is.na(forward) && forward >= 0.8) return(list(status = "forward", strand_specific = 1L, note = "Automatic strandedness detection suggests forward-stranded library."))
  if (!is.na(reverse) && reverse >= 0.8) return(list(status = "reverse", strand_specific = 2L, note = "Automatic strandedness detection suggests reverse-stranded library."))
  list(status = "unknown", strand_specific = 0L, note = "Strandedness could not be determined confidently; defaulting to unstranded. Please confirm manually.")
}

validate_design_formula <- function(design_formula, metadata_df) {
  formula_object <- as.formula(design_formula)
  variables <- all.vars(formula_object)
  missing <- setdiff(variables, names(metadata_df))
  if (length(missing) > 0) stop("Design formula references missing metadata columns: ", paste(missing, collapse = ", "), call. = FALSE)
  formula_object
}

markdown_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(c("| status | detail |", "| --- | --- |", "| none | no records |"))
  }
  header <- paste("|", paste(names(df), collapse = " | "), "|")
  separator <- paste("|", paste(rep("---", ncol(df)), collapse = " | "), "|")
  rows <- apply(df, 1, function(row) paste("|", paste(as.character(row), collapse = " | "), "|"))
  c(header, separator, rows)
}

detect_gene_id_type <- function(gene_ids) {
  ids <- unique(trimws(as.character(gene_ids)))
  ids <- ids[nzchar(ids)]
  ids <- sub("\\.[0-9]+$", "", ids)
  if (length(ids) == 0) return("SYMBOL")
  if (all(grepl("^ENSMUSG", ids))) return("ENSEMBL")
  if (all(grepl("^[0-9]+$", ids))) return("ENTREZID")
  "SYMBOL"
}
