suppressPackageStartupMessages({
  library(data.table)
})

get_current_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) == 0L) {
    return(getwd())
  }
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
}

load_first_nonmac_anchor_ops <- function(
  raw_path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/",
  extra_cols = character()
) {
  base_cols <- c(
    "op_id", "subject_id", "hadm_id", "case_id", "opdate", "antype",
    "admission_time", "discharge_time", "orin_time", "orout_time",
    "opstart_time", "anstart_time"
  )
  select_cols <- unique(c(base_cols, extra_cols))

  ops <- fread(
    file.path(raw_path, "operations.csv"),
    select = select_cols,
    na.strings = c("", "NA")
  )

  numeric_candidates <- intersect(
    c(
      "hadm_id", "admission_time", "discharge_time", "orin_time", "orout_time",
      "opstart_time", "anstart_time", "opdate"
    ),
    names(ops)
  )
  if (length(numeric_candidates) > 0L) {
    ops[, (numeric_candidates) := lapply(.SD, as.numeric), .SDcols = numeric_candidates]
  }

  ops[, antype_clean := toupper(trimws(as.character(antype)))]
  ops[, non_mac_flag := !is.na(antype_clean) & antype_clean != "MAC"]
  ops[, hadm_group := fifelse(is.na(hadm_id), paste0("MISSING_HADM_", op_id), as.character(hadm_id))]
  ops[, anchor_sort_time := fcoalesce(opstart_time, anstart_time, orin_time, opdate, admission_time)]

  anchor_ops <- ops[
    !is.na(subject_id) & !is.na(op_id) & !is.na(orin_time) & non_mac_flag
  ][order(subject_id, hadm_group, anchor_sort_time, op_id)][
    , .SD[1], by = .(subject_id, hadm_group)
  ]

  setorderv(anchor_ops, c("subject_id", "hadm_id", "op_id"))
  anchor_ops[]
}

write_anchor_map <- function(anchor_ops, output_dir, file_name = "anchor_first_nonMAC_operations.csv") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  keep_cols <- intersect(
    c(
      "subject_id", "hadm_id", "op_id", "case_id", "opdate",
      "antype", "antype_clean", "admission_time", "discharge_time",
      "orin_time", "orout_time", "anchor_sort_time"
    ),
    names(anchor_ops)
  )
  fwrite(anchor_ops[, ..keep_cols], file.path(output_dir, file_name))
}
