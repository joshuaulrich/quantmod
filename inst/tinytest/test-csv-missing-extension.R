library(quantmod)

missing_csv_message <- function(extension, use.lookup = FALSE) {
  old <- options("getSymbols.sources")
  on.exit(options(old))
  symbol <- "QUANTMOD_NONEXISTENT"
  folder <- tempfile("quantmod-csv-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  if (use.lookup) {
    setSymbolLookup(QUANTMOD_NONEXISTENT = list(extension = extension))
  }
  output <- capture.output(
    getSymbols(symbol, src = "csv", dir = folder,
               extension = if (use.lookup) "csv" else extension,
               env = new.env())
  )
  paste(output, collapse = "\n")
}

expect_true(grepl("QUANTMOD_NONEXISTENT.sparky", missing_csv_message("sparky"),
                 fixed = TRUE))
expect_true(grepl("QUANTMOD_NONEXISTENT.dat", missing_csv_message("dat", TRUE),
                 fixed = TRUE))
expect_true(grepl("QUANTMOD_NONEXISTENT.csv", missing_csv_message("csv"),
                 fixed = TRUE))
