suppressPackageStartupMessages({
  library(data.table)
})

script_dir <- getwd()
cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0L) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
}

script_list <- c(
  "Process_Demographics_and_Timeline_v2_first_nonMAC.R",
  "Diagnosis_v2_word_comorbidities_first_nonMAC.R",
  "build_comorbidity_summary_cn_first_nonMAC.R",
  "Acute_Status_3mo_v2_first_nonMAC_sepsis_A40_A41.R",
  "Medicine_pro_v2_word_first_nonMAC.R",
  "Lab_v2_first_nonMAC.R",
  "Vials_pro_v2_first_nonMAC.R",
  "Vials_intra_v2_first_nonMAC.R",
  "Vials_postop_v1_first_nonMAC.R",
  "Vials_intra_summary_v2_first_nonMAC.R",
  "Vials_intra_timeseries_qc_v1_first_nonMAC.R",
  "Vials_intra_timeseries_usage_and_cleaning_report_v1_first_nonMAC.R",
  "Outcome_v3_word_complications_first_nonMAC_sepsis_A40_A41.R"
)

run_one <- function(script_name) {
  script_path <- file.path(script_dir, script_name)
  if (!file.exists(script_path)) {
    stop(sprintf("Missing script: %s", script_path))
  }
  cat(sprintf("\n===== Running %s =====\n", script_name))
  out <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", script_path),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    cat(paste(out, collapse = "\n"), "\n")
    stop(sprintf("Word-aligned pipeline failed at %s", script_name))
  }
}

for (script_name in script_list) {
  run_one(script_name)
}

cat("\nWord-aligned first_nonMAC pipeline completed.\n")
