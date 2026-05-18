suppressPackageStartupMessages({
  library(data.table)
})

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required.")
}

processed_root <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed"
excel_row_limit <- 1048576L

module_dirs <- c(
  "baseline",
  "demographics",
  "diagnosis",
  "labs",
  "medications",
  "preop_vitals",
  "intraop",
  "outcomes",
  "master"
)

safe_sheet_name <- function(x, used) {
  nm <- sub("\\.csv$", "", basename(x))
  nm <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", nm)
  nm <- substr(nm, 1, 31)
  if (!(nm %in% used)) return(nm)
  i <- 2L
  repeat {
    suffix <- paste0("_", i)
    nm2 <- substr(nm, 1, max(1L, 31L - nchar(suffix)))
    nm2 <- paste0(nm2, suffix)
    if (!(nm2 %in% used)) return(nm2)
    i <- i + 1L
  }
}

count_rows_fast <- function(csv_path) {
  n <- suppressWarnings(as.integer(system(sprintf("wc -l < '%s'", csv_path), intern = TRUE)))
  if (is.na(n)) return(NA_integer_)
  max(0L, n - 1L)
}

write_module_workbook <- function(module_dir) {
  module_path <- file.path(processed_root, module_dir)
  if (!dir.exists(module_path)) return(invisible(NULL))

  csv_files <- sort(list.files(module_path, pattern = "\\.csv$", full.names = TRUE))
  if (length(csv_files) == 0L) return(invisible(NULL))

  wb <- openxlsx::createWorkbook()
  used_sheets <- character()
  include_log <- list()
  skip_log <- list()

  for (csv_path in csv_files) {
    row_n <- count_rows_fast(csv_path)
    col_n <- ncol(fread(csv_path, nrows = 0L))

    if (!is.na(row_n) && row_n > excel_row_limit) {
      skip_log[[length(skip_log) + 1L]] <- data.table(
        file_name = basename(csv_path),
        rows = row_n,
        cols = col_n,
        reason = sprintf("exceeds_excel_row_limit_%d", excel_row_limit)
      )
      next
    }

    dt <- fread(csv_path)
    sheet_name <- safe_sheet_name(csv_path, used_sheets)
    used_sheets <- c(used_sheets, sheet_name)

    openxlsx::addWorksheet(wb, sheetName = sheet_name)
    openxlsx::writeDataTable(wb, sheet = sheet_name, x = as.data.frame(dt), withFilter = TRUE)
    openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(dt), widths = "auto")

    include_log[[length(include_log) + 1L]] <- data.table(
      file_name = basename(csv_path),
      sheet_name = sheet_name,
      rows = nrow(dt),
      cols = ncol(dt)
    )
  }

  readme <- rbindlist(
    c(
      if (length(include_log) > 0L) list(rbindlist(include_log, fill = TRUE)[, status := "included"]),
      if (length(skip_log) > 0L) list(rbindlist(skip_log, fill = TRUE)[, `:=`(status = "skipped", sheet_name = NA_character_)])
    ),
    fill = TRUE
  )

  if (nrow(readme) > 0L) {
    setcolorder(readme, c("status", "file_name", "sheet_name", "rows", "cols", setdiff(names(readme), c("status", "file_name", "sheet_name", "rows", "cols"))))
    openxlsx::addWorksheet(wb, sheetName = "readme")
    openxlsx::writeDataTable(wb, sheet = "readme", x = as.data.frame(readme), withFilter = TRUE)
    openxlsx::freezePane(wb, sheet = "readme", firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet = "readme", cols = 1:ncol(readme), widths = "auto")
  }

  workbook_path <- file.path(module_path, sprintf("%s_tables.xlsx", module_dir))
  openxlsx::saveWorkbook(wb, file = workbook_path, overwrite = TRUE)
  message(sprintf("[OK] %s", workbook_path))
}

for (md in module_dirs) {
  write_module_workbook(md)
}

message("Done.")
