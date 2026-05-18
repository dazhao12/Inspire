#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

processed_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed"
operations_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/operations.csv"

if (!file.exists(operations_path)) {
  stop("operations.csv not found: ", operations_path)
}

id_map <- unique(fread(operations_path, select = c("op_id", "subject_id", "hadm_id")))
setkey(id_map, op_id)

csv_files <- list.files(processed_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0L) {
  stop("No CSV files found in: ", processed_dir)
}

for (f in csv_files) {
  nm <- basename(f)
  hdr <- names(fread(f, nrows = 0L, showProgress = FALSE))

  # Subject-level table: keep as subject-level and sort by subject_id.
  if (!("op_id" %in% hdr) && ("subject_id" %in% hdr)) {
    dt <- fread(f, showProgress = FALSE)
    setorderv(dt, "subject_id", na.last = TRUE)
    fwrite(dt, f)
    cat("[subject-level sorted]", nm, "\n")
    next
  }

  # Non-operation table (no op_id and no subject_id): skip.
  if (!("op_id" %in% hdr)) {
    cat("[skipped - no op_id]", nm, "\n")
    next
  }

  dt <- fread(f, showProgress = FALSE)

  missing_subject <- !("subject_id" %in% names(dt))
  missing_hadm <- !("hadm_id" %in% names(dt))

  if (missing_subject || missing_hadm) {
    add_cols <- c("op_id")
    if (missing_subject) add_cols <- c(add_cols, "subject_id")
    if (missing_hadm) add_cols <- c(add_cols, "hadm_id")
    add_map <- unique(id_map[, ..add_cols])
    dt <- merge(dt, add_map, by = "op_id", all.x = TRUE, sort = FALSE)
    cat("[id columns added]", nm, "subject_added=", missing_subject,
        "hadm_added=", missing_hadm, "\n")
  }

  id_cols <- c("subject_id", "hadm_id", "op_id")
  ordered_cols <- c(id_cols[id_cols %in% names(dt)], setdiff(names(dt), id_cols))
  setcolorder(dt, ordered_cols)

  sort_keys <- id_cols[id_cols %in% names(dt)]
  setorderv(dt, sort_keys, na.last = TRUE)
  fwrite(dt, f)
  cat("[operation-level standardized]", nm, "\n")
}

cat("Done.\n")
