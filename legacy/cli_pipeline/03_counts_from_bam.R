source("scripts/common.R")

config <- load_config("config.yaml")
paths <- resolve_paths(config)
ensure_dirs(planned_result_dirs(paths))

candidate_path <- file.path(paths$inventory_dir, "candidate_samples.tsv")
count_matrix_path <- file.path(paths$counts_dir, "raw_gene_counts.csv")
summary_path <- file.path(paths$counts_dir, "featurecounts_summary.csv")
params_path <- file.path(paths$counts_dir, "featurecounts_params.tsv")
report_path <- file.path(paths$counts_dir, "count_run_report.md")

assert_no_overwrite(count_matrix_path)
assert_no_overwrite(summary_path)
assert_no_overwrite(params_path)
assert_no_overwrite(report_path)

if (!file.exists(candidate_path)) stop("Candidate sample table not found. Run scripts/02_inventory.R first.", call. = FALSE)
if (!file.exists(paths$metadata)) stop("Metadata file not found. Run scripts/02_inventory.R first to generate a template.", call. = FALSE)

samtools_path <- command_path("samtools", config = config, config_field = "samtools_path")
if (!nzchar(samtools_path)) stop("samtools is required for BAM quickcheck and pairing detection before counting. Configure samtools_path and re-run.", call. = FALSE)

inventory_df <- read.delim(candidate_path, stringsAsFactors = FALSE, check.names = FALSE)
if (nrow(inventory_df) == 0) stop("No valid BAM candidates were found in inventory.", call. = FALSE)
if (any(inventory_df$quickcheck_status != "pass")) stop("All BAM candidates must pass samtools quickcheck before counting.", call. = FALSE)

metadata_df <- read.csv(paths$metadata, stringsAsFactors = FALSE, check.names = FALSE)
metadata_df <- validate_metadata(metadata_df, inventory_df = inventory_df, require_design = FALSE)
metadata_df <- metadata_df[match(metadata_df$bam_file, inventory_df$bam_file), , drop = FALSE]
metadata_df <- metadata_df[!is.na(metadata_df$bam_file), , drop = FALSE]
metadata_df$bam_path <- file.path(paths$bam_dir, metadata_df$bam_file)

pairing_info <- do.call(rbind, lapply(seq_len(nrow(metadata_df)), function(i) {
  pairing <- infer_pairing_status(metadata_df$bam_path[[i]], samtools_path)
  data.frame(sample_id = metadata_df$sample_id[[i]], bam_file = metadata_df$bam_file[[i]], pairing_status = pairing$status, pairing_ratio = pairing$ratio, pairing_reason = pairing$reason, stringsAsFactors = FALSE)
}))

if (any(pairing_info$pairing_status %in% c("unknown", "mixed"))) stop("Pairing detection was not conclusive for all BAM files. Review BAM integrity before counting.", call. = FALSE)
paired_flags <- unique(pairing_info$pairing_status == "paired_end")
if (length(paired_flags) != 1) stop("Mixed single-end and paired-end libraries are not supported in one count run.", call. = FALSE)
is_paired <- isTRUE(paired_flags[[1]])

strand_info <- infer_strandedness(config, paths, metadata_df$bam_path)
engine <- select_count_engine(config)
if (is.na(engine$engine)) stop("No counting engine available. Install featureCounts or ensure Rsubread is available.", call. = FALSE)

annotation_file <- prepare_annotation_for_counting(paths$gtf_file, engine$engine, paths$tmp_dir)

if (engine$engine == "Rsubread") {
  suppressPackageStartupMessages(library(Rsubread))
  fc <- Rsubread::featureCounts(files = metadata_df$bam_path, annot.ext = annotation_file, isGTFAnnotationFile = TRUE, GTF.featureType = "exon", GTF.attrType = "gene_id", useMetaFeatures = TRUE, isPairedEnd = is_paired, strandSpecific = strand_info$strand_specific, nthreads = 1)
  counts_df <- data.frame(gene_id = rownames(fc$counts), fc$counts, row.names = NULL, check.names = FALSE)
  colnames(counts_df) <- c("gene_id", metadata_df$sample_id)
  summary_df <- data.frame(status = rownames(fc$stat), fc$stat, row.names = NULL, check.names = FALSE)
  colnames(summary_df) <- c("status", metadata_df$sample_id)
} else {
  raw_output <- file.path(paths$tmp_dir, "featurecounts_raw.txt")
  if (file.exists(raw_output) || file.exists(paste0(raw_output, ".summary"))) stop("Temporary featureCounts output already exists in results/tmp. Remove it before rerunning.", call. = FALSE)
  args <- c("-a", annotation_file, "-o", raw_output, "-T", "1", "-t", "exon", "-g", "gene_id", "-s", as.character(strand_info$strand_specific))
  if (is_paired) args <- c(args, "-p")
  args <- c(args, metadata_df$bam_path)
  result <- run_command(engine$command, args)
  if (result$status != 0L) stop("featureCounts failed: ", paste(result$output, collapse = "\n"), call. = FALSE)
  raw_counts <- read.delim(raw_output, comment.char = "#", stringsAsFactors = FALSE, check.names = FALSE)
  summary_raw <- read.delim(paste0(raw_output, ".summary"), stringsAsFactors = FALSE, check.names = FALSE)
  counts_only <- raw_counts[, metadata_df$bam_path, drop = FALSE]
  colnames(counts_only) <- metadata_df$sample_id
  counts_df <- data.frame(gene_id = raw_counts$Geneid, counts_only, check.names = FALSE)
  summary_only <- summary_raw[, metadata_df$bam_path, drop = FALSE]
  colnames(summary_only) <- metadata_df$sample_id
  summary_df <- data.frame(status = summary_raw$Status, summary_only, check.names = FALSE)
}

params_df <- merge(metadata_df[, c("sample_id", "bam_file")], pairing_info, by = c("sample_id", "bam_file"), all.x = TRUE, sort = FALSE)
params_df$count_engine <- engine$engine
params_df$annotation_file <- annotation_file
params_df$is_paired_end <- is_paired
params_df$strandedness_status <- strand_info$status
params_df$strandedness_note <- strand_info$note
params_df$samtools_path <- samtools_path

report_lines <- c(
  "# Count Run Report",
  "",
  paste("- Generated:", timestamp_now()),
  paste("- samtools path:", samtools_path),
  paste("- Counting engine:", engine$engine),
  paste("- Annotation file used:", annotation_file),
  paste("- Paired-end mode:", is_paired),
  paste("- Strandedness status:", strand_info$status),
  paste("- Strandedness note:", strand_info$note),
  "",
  "## FeatureCounts Parameters",
  "",
  markdown_table(params_df)
)

write_csv_safe(counts_df, count_matrix_path, row.names = FALSE)
write_csv_safe(summary_df, summary_path, row.names = FALSE)
write_tsv_safe(params_df, params_path, row.names = FALSE)
write_lines_safe(report_lines, report_path)