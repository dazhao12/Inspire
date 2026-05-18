#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

raw_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/01_raw/operations.csv"
out_op <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/demographic_operation_latest.csv"
out_subject <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/demographic_subject_latest.csv"

dir.create(dirname(out_op), recursive = TRUE, showWarnings = FALSE)

cat("Reading operations.csv ...\n")
ops <- fread(
  raw_path,
  select = c(
    "subject_id", "hadm_id", "op_id", "opdate",
    "sex", "age", "height", "weight", "race", "asa",
    "emop", "department", "antype", "icd10_pcs"
  ),
  na.strings = c("", "NA")
)

cat("Building operation-level demographic table (all operations) ...\n")
ops[, Male := fifelse(sex == "M", 1,
               fifelse(sex == "F", 0, NA_real_))]
ops[, Age := suppressWarnings(as.numeric(age))]
ops[, Height := suppressWarnings(as.numeric(height))]
ops[, Weight := suppressWarnings(as.numeric(weight))]
ops[, BMI := fifelse(!is.na(Height) & Height > 0 & !is.na(Weight),
                     Weight / ((Height / 100)^2), NA_real_)]
ops[, Emergency_op := emop]

op_out <- ops[, .(
  subject_id, hadm_id, op_id, opdate, Male, Age, Height, Weight, BMI,
  race, asa, Emergency_op, department, antype, icd10_pcs
)]
setorder(op_out, subject_id, hadm_id, op_id)

cat("Building subject-level demographic table (grouped by subject_id) ...\n")
first_non_na_num <- function(x) {
  idx <- which(!is.na(x))
  if (length(idx) == 0L) return(NA_real_)
  as.numeric(x[idx[1]])
}

first_non_na_chr <- function(x) {
  idx <- which(!is.na(x) & x != "")
  if (length(idx) == 0L) return(NA_character_)
  as.character(x[idx[1]])
}

subject_out <- op_out[, .(
  Age = first_non_na_num(Age),
  Height = first_non_na_num(Height),
  Weight = first_non_na_num(Weight),
  BMI = first_non_na_num(BMI),
  Male = first_non_na_num(Male),
  race = first_non_na_chr(race),
  n_anchor_admissions = .N
), by = .(subject_id)]
setorder(subject_out, subject_id)

fwrite(op_out, out_op)
fwrite(subject_out, out_subject)

cat("Done.\n")
cat("Operation output: ", out_op, "\n", sep = "")
cat("Subject output: ", out_subject, "\n", sep = "")
cat("Rows(operation): ", nrow(op_out), ", Cols: ", ncol(op_out), "\n", sep = "")
cat("Rows(subject): ", nrow(subject_out), ", Cols: ", ncol(subject_out), "\n", sep = "")
