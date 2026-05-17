library(data.table)

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required.")
}

project_root <- "/N/project/analgesia_perioperation"
processed_dir <- file.path(project_root, "data", "INSPIRE_1.3", "processed")
docs_dir <- file.path(project_root, "documents", "INSPIRE_1.3", "processing_docs", as.character(Sys.Date()))

if (!dir.exists(docs_dir)) {
  dir.create(docs_dir, recursive = TRUE)
}

files <- sort(list.files(processed_dir, pattern = "[.]csv$", full.names = TRUE))
if (length(files) == 0L) {
  stop("No CSV files found under processed directory.")
}

file_level <- vector("list", length(files))
column_level <- vector("list", length(files))

for (i in seq_along(files)) {
  f <- files[[i]]
  dt <- fread(f, showProgress = FALSE)

  n_rows <- nrow(dt)
  n_cols <- ncol(dt)
  col_names <- names(dt)

  miss_n <- vapply(dt, function(x) sum(is.na(x)), integer(1))
  miss_pct <- round(100 * miss_n / pmax(n_rows, 1L), 2)
  non_miss_n <- n_rows - miss_n
  unique_n <- vapply(dt, function(x) uniqueN(x, na.rm = TRUE), integer(1))
  col_class <- vapply(dt, function(x) class(x)[1], character(1))

  file_level[[i]] <- data.table(
    file_name = basename(f),
    n_rows = n_rows,
    n_cols = n_cols,
    total_cells = as.double(n_rows) * as.double(n_cols),
    missing_cells = sum(miss_n),
    missing_cells_pct = round(100 * sum(miss_n) / pmax(as.double(n_rows) * as.double(n_cols), 1), 2),
    cols_all_missing_n = sum(miss_n == n_rows),
    cols_no_missing_n = sum(miss_n == 0L)
  )

  column_level[[i]] <- data.table(
    file_name = basename(f),
    column_name = col_names,
    column_class = col_class,
    n_rows = n_rows,
    non_missing_n = non_miss_n,
    missing_n = miss_n,
    missing_pct = miss_pct,
    unique_non_missing_n = unique_n
  )
}

file_summary_dt <- rbindlist(file_level, use.names = TRUE)
setorder(file_summary_dt, -missing_cells_pct, file_name)

column_summary_dt <- rbindlist(column_level, use.names = TRUE)
setorder(column_summary_dt, file_name, -missing_pct, column_name)

high_missing_dt <- column_summary_dt[missing_pct >= 50]
setorder(high_missing_dt, -missing_pct, file_name, column_name)

out_xlsx <- file.path(docs_dir, "processed_missing_summary.xlsx")
out_csv <- file.path(docs_dir, "processed_missing_summary_file_level.csv")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "file_summary")
openxlsx::writeDataTable(wb, "file_summary", as.data.frame(file_summary_dt), withFilter = TRUE)
openxlsx::freezePane(wb, "file_summary", firstRow = TRUE)

openxlsx::addWorksheet(wb, "column_missing")
openxlsx::writeDataTable(wb, "column_missing", as.data.frame(column_summary_dt), withFilter = TRUE)
openxlsx::freezePane(wb, "column_missing", firstRow = TRUE)

openxlsx::addWorksheet(wb, "high_missing_ge50pct")
openxlsx::writeDataTable(wb, "high_missing_ge50pct", as.data.frame(high_missing_dt), withFilter = TRUE)
openxlsx::freezePane(wb, "high_missing_ge50pct", firstRow = TRUE)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
fwrite(file_summary_dt, out_csv)

cat("Saved:\n")
cat(out_xlsx, "\n")
cat(out_csv, "\n")
