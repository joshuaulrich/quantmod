# run package unit tests
if (requireNamespace("tinytest", quietly = TRUE)) {
    suppressPackageStartupMessages(library("quantmod"))
    use_color <- as.logical(Sys.getenv("_PKG_TINYTEST_COLOR_", FALSE))
    verbosity <- as.integer(Sys.getenv("_PKG_TINYTEST_VERBOSE_", 1))
    cat("tinytest colored output:", use_color,
        "\ntinytest verbosity:", verbosity, "\n")
    tinytest::test_package("quantmod", color = use_color, verbose = verbosity)
}
