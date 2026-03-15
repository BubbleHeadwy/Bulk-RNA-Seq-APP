required_sample_sheet_columns <- function(include_batch = FALSE) {
  base <- c("sample_id", "bam_file", "condition", "genotype", "replicate")
  if (isTRUE(include_batch)) c(base, "batch") else base
}

validate_gtf_file <- function(gtf_file) {
  path <- trimws(as.character(gtf_file %||% ""))
  if (!nzchar(path)) {
    return(list(ok = FALSE, message = "请选择 GTF 文件。", info = NULL))
  }
  if (!file.exists(path)) {
    return(list(ok = FALSE, message = "所选 GTF 文件不存在。", info = NULL))
  }
  if (!grepl("\\.gtf(\\.gz)?$", path, ignore.case = TRUE)) {
    return(list(ok = FALSE, message = "所选 GTF 文件格式不正确，仅支持 .gtf 或 .gtf.gz。", info = NULL))
  }

  info <- file.info(path)
  preview <- tryCatch({
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, open = "rt") else file(path, open = "rt")
    on.exit(close(con), add = TRUE)
    readLines(con, n = 6, warn = FALSE)
  }, error = function(e) character(0))

  list(
    ok = TRUE,
    message = "GTF 文件验证通过。",
    info = list(
      path = normalizePath(path, winslash = "/", mustWork = FALSE),
      size = unname(info$size),
      compressed = grepl("\\.gz$", path, ignore.case = TRUE),
      preview = preview
    )
  )
}

validate_sample_sheet_file <- function(sample_sheet, include_batch = FALSE) {
  path <- trimws(as.character(sample_sheet %||% ""))
  if (!nzchar(path)) {
    return(list(ok = FALSE, message = "请选择 sample sheet。", data = NULL))
  }
  if (!file.exists(path)) {
    return(list(ok = FALSE, message = "sample sheet 不存在。", data = NULL))
  }

  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) e)
  if (inherits(df, "error")) {
    return(list(ok = FALSE, message = paste("sample sheet 无法读取：", conditionMessage(df)), data = NULL))
  }

  required <- required_sample_sheet_columns(include_batch = include_batch)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    return(list(ok = FALSE, message = paste("sample sheet 缺少必需列：", paste(missing, collapse = ", ")), data = df))
  }

  df[] <- lapply(df, function(column) {
    if (is.character(column)) {
      out <- trimws(column)
      out[is.na(out)] <- ""
      out
    } else {
      column
    }
  })

  list(ok = TRUE, message = "sample sheet 验证通过。", data = df)
}

validate_bam_inputs <- function(bam_dir, sample_sheet = NULL, include_batch = FALSE) {
  bam_dir <- trimws(as.character(bam_dir %||% ""))
  if (!nzchar(bam_dir)) {
    return(list(ok = FALSE, message = "请选择 BAM 目录。", bam_table = NULL, metadata = NULL))
  }
  if (!dir.exists(bam_dir)) {
    return(list(ok = FALSE, message = "所选 BAM 目录不存在。", bam_table = NULL, metadata = NULL))
  }

  files <- list.files(bam_dir, full.names = TRUE, recursive = FALSE, no.. = TRUE)
  if (length(files) == 0) {
    return(list(ok = FALSE, message = "未检测到有效的 BAM 文件，请确认目录。", bam_table = NULL, metadata = NULL))
  }

  basenames <- basename(files)
  is_bad <- grepl("\\.BAD$", basenames, ignore.case = TRUE)
  is_bam <- grepl("\\.bam$", basenames, ignore.case = TRUE)

  bam_table <- data.frame(
    sample_id = ifelse(is_bam, tools::file_path_sans_ext(basenames), NA_character_),
    bam_file = basenames,
    bam_path = normalizePath(files, winslash = "/", mustWork = FALSE),
    is_bam = is_bam,
    is_bad = is_bad,
    bai_exists = FALSE,
    stringsAsFactors = FALSE
  )
  bam_table$bai_path <- paste0(bam_table$bam_path, ".bai")
  bam_table$bai_exists[bam_table$is_bam] <- file.exists(bam_table$bai_path[bam_table$is_bam])

  valid_bams <- bam_table[bam_table$is_bam & !bam_table$is_bad, , drop = FALSE]
  if (nrow(valid_bams) == 0) {
    return(list(ok = FALSE, message = "未检测到有效的 BAM 文件，请确认目录。", bam_table = bam_table, metadata = NULL))
  }
  if (any(!valid_bams$bai_exists)) {
    missing <- valid_bams$bam_file[!valid_bams$bai_exists]
    return(list(
      ok = FALSE,
      message = paste("以下 BAM 缺少对应的 .bam.bai 索引：", paste(missing, collapse = ", ")),
      bam_table = bam_table,
      metadata = NULL
    ))
  }

  metadata_result <- validate_sample_sheet_file(sample_sheet, include_batch = include_batch)
  if (!isTRUE(metadata_result$ok)) {
    return(list(ok = FALSE, message = metadata_result$message, bam_table = bam_table, metadata = metadata_result$data))
  }

  metadata_df <- metadata_result$data
  missing_bam <- setdiff(metadata_df$bam_file, valid_bams$bam_file)
  if (length(missing_bam) > 0) {
    return(list(
      ok = FALSE,
      message = paste("sample sheet 中引用了未在 BAM 目录中发现的文件：", paste(missing_bam, collapse = ", ")),
      bam_table = bam_table,
      metadata = metadata_df
    ))
  }

  list(ok = TRUE, message = "BAM 与 sample sheet 验证通过。", bam_table = bam_table, metadata = metadata_df)
}

validate_featurecounts_matrix <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    return(list(ok = FALSE, message = "未找到 raw_gene_counts.csv。", data = NULL))
  }
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) e)
  if (inherits(df, "error")) {
    return(list(ok = FALSE, message = paste("raw_gene_counts.csv 无法读取：", conditionMessage(df)), data = NULL))
  }
  if (!"gene_id" %in% names(df)) {
    return(list(ok = FALSE, message = "raw_gene_counts.csv 缺少 gene_id 列。", data = df))
  }
  if (ncol(df) < 2) {
    return(list(ok = FALSE, message = "raw_gene_counts.csv 没有样本列。", data = df))
  }
  list(ok = TRUE, message = "raw_gene_counts.csv 结构验证通过。", data = df)
}

validate_results_bundle <- function(results_dir, sample_sheet = NULL) {
  results_dir <- trimws(as.character(results_dir %||% ""))
  if (!nzchar(results_dir)) {
    return(list(ok = FALSE, message = "请选择 results 目录。", details = NULL))
  }
  if (!dir.exists(results_dir)) {
    return(list(ok = FALSE, message = "所选 results 目录不存在。", details = NULL))
  }

  paths <- list(
    counts = file.path(results_dir, "02_counts", "raw_gene_counts.csv"),
    normalized = file.path(results_dir, "03_deseq2", "normalized_counts.csv"),
    deseq = file.path(results_dir, "03_deseq2", "deseq2_results.csv"),
    annotated = file.path(results_dir, "06_annotation", "annotated_deseq2_results.csv"),
    go = file.path(results_dir, "07_enrichment", "go_enrichment_results.csv")
  )

  count_check <- validate_featurecounts_matrix(paths$counts)
  if (!isTRUE(count_check$ok)) {
    return(list(ok = FALSE, message = count_check$message, details = paths))
  }

  if (!file.exists(paths$normalized)) {
    return(list(ok = FALSE, message = "未找到 normalized_counts.csv。", details = paths))
  }

  metadata_result <- validate_sample_sheet_file(sample_sheet, include_batch = FALSE)
  if (!isTRUE(metadata_result$ok)) {
    return(list(ok = FALSE, message = metadata_result$message, details = paths))
  }

  if (file.exists(paths$deseq)) {
    deseq_df <- tryCatch(read.csv(paths$deseq, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (!is.null(deseq_df) && !all(c("gene_id", "log2FoldChange", "padj") %in% names(deseq_df))) {
      return(list(ok = FALSE, message = "DESeq2 结果文件结构不完整。", details = paths))
    }
  }

  if (file.exists(paths$annotated)) {
    annotated_df <- tryCatch(read.csv(paths$annotated, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (!is.null(annotated_df) && !all(c("gene_id") %in% names(annotated_df))) {
      return(list(ok = FALSE, message = "注释结果文件结构不完整。", details = paths))
    }
  }

  if (file.exists(paths$go)) {
    go_df <- tryCatch(read.csv(paths$go, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (is.null(go_df)) {
      return(list(ok = FALSE, message = "GO 富集结果文件无法读取。", details = paths))
    }
  }

  list(ok = TRUE, message = "results 目录验证通过。", details = paths)
}

preview_table <- function(df, n = 10) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(提示 = "暂无可预览数据", stringsAsFactors = FALSE))
  }
  utils::head(df, n)
}

safe_read_csv <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
}

validate_formula_string <- function(formula_text, metadata_df) {
  if (!nzchar(trimws(formula_text %||% ""))) {
    return(list(ok = FALSE, message = "设计公式不能为空。", formula = NULL))
  }
  parsed <- tryCatch(as.formula(formula_text), error = function(e) e)
  if (inherits(parsed, "error")) {
    return(list(ok = FALSE, message = paste("设计公式格式错误：", conditionMessage(parsed)), formula = NULL))
  }
  missing <- setdiff(all.vars(parsed), names(metadata_df))
  if (length(missing) > 0) {
    return(list(ok = FALSE, message = paste("设计公式中包含不存在的列：", paste(missing, collapse = ", ")), formula = NULL))
  }
  list(ok = TRUE, message = "设计公式验证通过。", formula = parsed)
}

apply_sample_selection <- function(metadata_df, selected_sample_ids) {
  if (is.null(metadata_df) || nrow(metadata_df) == 0) {
    return(metadata_df)
  }
  ids <- unique(as.character(selected_sample_ids %||% character(0)))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) {
    return(metadata_df[0, , drop = FALSE])
  }
  metadata_df[metadata_df$sample_id %in% ids, , drop = FALSE]
}
