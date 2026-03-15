source("scripts/common.R")

config <- load_config("config.yaml")
paths <- resolve_paths(config)
ensure_dirs(c("scripts", paths$metadata_dir, "reports", planned_result_dirs(paths)))

setup_report <- file.path(paths$logs_dir, "setup_report.md")
assert_no_overwrite(setup_report)

command_fields <- list(samtools = "samtools_path", featureCounts = "featurecounts_path", `infer_experiment.py` = "infer_experiment_path")
commands <- collect_command_status(c("Rscript", "R.exe", "samtools", "featureCounts", "infer_experiment.py"), config = config, config_fields = command_fields)
packages <- collect_package_status(c("yaml", "DESeq2", "tidyverse", "ggplot2", "pheatmap", "clusterProfiler", "AnnotationDbi", "Rsubread", "org.Mm.eg.db"))
references <- path_status(list(
  raw_data_dir = paths$raw_data_dir,
  bam_dir = paths$bam_dir,
  reference_dir = paths$reference_dir,
  gtf_file = paths$gtf_file,
  genome_fasta = paths$genome_fasta,
  gene_fasta = paths$gene_fasta,
  metadata_dir = paths$metadata_dir,
  output_dir = paths$output_dir
))

report_lines <- c(
  "# Setup Report",
  "",
  paste("- Generated:", timestamp_now()),
  paste("- Project root:", paths$repo_root),
  paste("- Configured R library:", trim_scalar(config$r_lib_path %||% "")),
  paste("- Configured samtools path:", trim_scalar(config$samtools_path %||% "")),
  "",
  "## Reference Paths",
  "",
  markdown_table(references),
  "",
  "## Command Availability",
  "",
  markdown_table(commands),
  "",
  "## R Package Availability",
  "",
  markdown_table(packages)
)

write_lines_safe(report_lines, setup_report)