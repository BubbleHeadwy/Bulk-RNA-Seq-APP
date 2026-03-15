source("scripts/common.R")

config <- load_config("config.yaml")
paths <- resolve_paths(config)
ensure_dirs(planned_result_dirs(paths))
ensure_dir(paths$metadata_dir)

inventory_report <- file.path(paths$inventory_dir, "inventory_report.md")
files_tsv <- file.path(paths$inventory_dir, "inventory_files.tsv")
candidates_tsv <- file.path(paths$inventory_dir, "candidate_samples.tsv")
metadata_status_md <- file.path(paths$inventory_dir, "metadata_status.md")

assert_no_overwrite(inventory_report)
assert_no_overwrite(files_tsv)
assert_no_overwrite(candidates_tsv)
assert_no_overwrite(metadata_status_md)

command_fields <- list(samtools = "samtools_path", featureCounts = "featurecounts_path", `infer_experiment.py` = "infer_experiment_path")
samtools_path <- command_path("samtools", config = config, config_field = "samtools_path")
inventory <- scan_bam_inventory(paths, config)
commands <- collect_command_status(c("Rscript", "R.exe", "samtools", "featureCounts", "infer_experiment.py"), config = config, config_fields = command_fields)
packages <- collect_package_status(c("yaml", "DESeq2", "tidyverse", "ggplot2", "pheatmap", "clusterProfiler", "AnnotationDbi", "Rsubread", "org.Mm.eg.db"))
references <- path_status(list(
  raw_data_dir = paths$raw_data_dir,
  bam_dir = paths$bam_dir,
  reference_dir = paths$reference_dir,
  gtf_file = paths$gtf_file,
  genome_fasta = paths$genome_fasta,
  gene_fasta = paths$gene_fasta
))

valid_candidates <- inventory$candidates
if (isTRUE(config$require_bai)) {
  valid_candidates <- valid_candidates[valid_candidates$bai_exists, , drop = FALSE]
}
quickcheck <- run_quickcheck(valid_candidates, samtools_path)
if (nrow(valid_candidates) > 0) {
  valid_candidates <- merge(valid_candidates, quickcheck, by = c("sample_id", "bam_file"), all.x = TRUE, sort = FALSE)
} else {
  valid_candidates$quickcheck_status <- character(0)
  valid_candidates$quickcheck_message <- character(0)
}
valid_candidates$usable_for_analysis <- valid_candidates$bai_exists & valid_candidates$quickcheck_status == "pass"
if (!nzchar(samtools_path)) {
  valid_candidates$usable_for_analysis <- FALSE
}

metadata_lines <- c("# Metadata Status", "")
if (file.exists(paths$metadata)) {
  validation_msg <- NULL
  metadata_df <- tryCatch({
    df <- read.csv(paths$metadata, stringsAsFactors = FALSE, check.names = FALSE)
    validate_metadata(df, inventory_df = valid_candidates, require_design = FALSE)
  }, error = function(e) {
    validation_msg <<- conditionMessage(e)
    NULL
  })
  if (is.null(validation_msg)) {
    complete_flag <- metadata_design_complete(metadata_df)
    metadata_lines <- c(
      metadata_lines,
      "- Existing sample_sheet.csv detected and passed structural validation.",
      paste("- Design columns complete:", complete_flag)
    )
  } else {
    metadata_lines <- c(metadata_lines, "- Existing sample_sheet.csv detected but failed validation.", paste("- Error:", validation_msg))
  }
} else {
  template <- generate_sample_sheet_template(valid_candidates)
  write.csv(template, file = paths$metadata, row.names = FALSE, quote = TRUE, na = "")
  metadata_lines <- c(
    metadata_lines,
    "- sample_sheet.csv did not exist and a template was generated.",
    paste("- Template path:", paths$metadata),
    "- condition, genotype, and replicate were intentionally left blank for manual completion."
  )
}

summary_df <- data.frame(
  metric = c("bam_files", "bam_index_files", "ignored_bad_files", "other_files", "valid_candidates", "missing_bai", "quickcheck_failures", "samtools_available"),
  value = c(
    sum(inventory$files$file_type == "bam"),
    sum(inventory$files$file_type == "bam_bai"),
    sum(inventory$files$file_type == "ignored_bad"),
    sum(inventory$files$file_type == "other"),
    nrow(valid_candidates),
    nrow(inventory$missing_bai),
    sum(valid_candidates$quickcheck_status == "fail", na.rm = TRUE),
    nzchar(samtools_path)
  ),
  stringsAsFactors = FALSE
)

ignored_bad <- inventory$files[inventory$files$file_type == "ignored_bad", c("file_name", "file_path"), drop = FALSE]
unexpected <- inventory$files[inventory$files$file_type == "other", c("file_name", "file_path"), drop = FALSE]
missing_bai <- inventory$missing_bai[, c("sample_id", "bam_file", "bai_file"), drop = FALSE]

report_lines <- c(
  "# Inventory Report",
  "",
  paste("- Generated:", timestamp_now()),
  paste("- BAM directory:", paths$bam_dir),
  paste("- Metadata path:", paths$metadata),
  paste("- Configured samtools path:", trim_scalar(config$samtools_path %||% "")),
  "",
  "## Dynamic Summary",
  "",
  markdown_table(summary_df),
  "",
  "## Candidate Samples",
  "",
  markdown_table(valid_candidates),
  "",
  "## Missing BAM Index Files",
  "",
  markdown_table(missing_bai),
  "",
  "## Ignored .BAD Files",
  "",
  markdown_table(ignored_bad),
  "",
  "## Other Files",
  "",
  markdown_table(unexpected),
  "",
  "## Reference Checks",
  "",
  markdown_table(references),
  "",
  "## Command Checks",
  "",
  markdown_table(commands),
  "",
  "## Package Checks",
  "",
  markdown_table(packages),
  "",
  "## Metadata",
  ""
)
report_lines <- c(report_lines, metadata_lines[-1])
if (!nzchar(samtools_path)) {
  report_lines <- c(report_lines, "", "## Blocking Note", "", "- `samtools` is not available, so `samtools quickcheck` could not run and downstream BAM-based analysis should remain blocked until `samtools` is installed.")
}

write_tsv_safe(inventory$files, files_tsv, row.names = FALSE)
write_tsv_safe(valid_candidates, candidates_tsv, row.names = FALSE)
write_lines_safe(metadata_lines, metadata_status_md)
write_lines_safe(report_lines, inventory_report)