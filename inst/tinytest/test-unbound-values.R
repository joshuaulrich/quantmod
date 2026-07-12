# Regression tests for previously unbound runtime values.
#
# Each case exercises a code path that once read an unbound variable
# (base::col in addExpiry, an undefined fr in getSymbols.IBrokers, or a
# not-yet-defaulted argument in getSymbols.oanda) and checks the intended,
# deterministic behaviour with no network access.
#
# See R/addTA.R, R/getSymbols.R and R/getModelData.R.

library(quantmod)
library(tinytest)
data(sample_matrix, package = "xts")
SM <- as.xts(sample_matrix)

# ---- addExpiry: stale `col` must not leak into params ---------------------
# addExpiry() stored `col = col` with no `col` formal, so the value silently
# became base::col(). chartExpiry renders from params$colors$Expiry and never
# reads params$col, so the fix drops the entry entirely.
before_devs <- dev.list()
chart_device <- tempfile(fileext = ".pdf")
pdf(file = chart_device)
chartSeries(SM, plot = FALSE)
# Call via a closure so addExpiry() takes its "return chobTA" branch instead
# of the redraw branch (which touches the graphics device).
ae <- (function() addExpiry())()
expect_false("col" %in% names(ae@params))          # red-before / green-after
expect_true(!is.null(ae@params$colors$Expiry))     # render source still present
for (d in setdiff(dev.list(), before_devs)) try(dev.off(d), silent = TRUE)
unlink(chart_device)

# ---- getSymbols.oanda: auto.assign must default before first use ---------
# The multi-symbol guard referenced auto.assign before its default was set,
# so a direct call without auto.assign errored with "object 'auto.assign'
# not found". A malformed (single) currency pair is skipped early in the
# loop, so no network request is made.
oe <- new.env()
res <- try(getSymbols.oanda("BADPAIR", env = oe), silent = TRUE)
expect_false(inherits(res, "try-error"))
expect_true(identical(as.character(res), "BADPAIR"))

# ---- getSymbols.IBrokers: fr must be defined on the all-fail path --------
# With no symbol resolving to a valid twsContract and auto.assign = FALSE,
# the final `return(fr)` once referenced a never-assigned fr. Mock the
# availability check and IBrokers connection helpers inside a local scope
# whose on.exit restores state even if the call errors, so no globals or
# namespace bindings leak. No IBrokers package or TWS connection is required.
run_ib_all_fail <- function() {
  orig_avail <- quantmod:::is.method.available
  assignInNamespace("is.method.available", function(method, package) TRUE,
                    ns = "quantmod")
  assign("twsConnect", function(clientId = 1001)
    structure(list(clientId = clientId), class = "twsconn"), envir = globalenv())
  assign("twsDisconnect", function(tws) invisible(NULL), envir = globalenv())
  on.exit({
    assignInNamespace("is.method.available", orig_avail, ns = "quantmod")
    suppressWarnings(rm(twsConnect, twsDisconnect, envir = globalenv()))
  })
  # No SymbolLookup entry => Contract is NULL => fr stays NULL (now init'd).
  quantmod:::getSymbols.IBrokers("FAKE", env = new.env(), auto.assign = FALSE)
}
ib <- try(run_ib_all_fail(), silent = TRUE)
# Pre-fix: try-error "object 'fr' not found". Post-fix: returns NULL.
expect_false(inherits(ib, "try-error"))

# ---- getModelData: empty model.spec must return gracefully ---------------
# With no variables in the spec, model.symbols is empty; pre-fix the function
# crashed with "subscript out of bounds" before reaching the merge loop.
nq <- new("quantmod")
nq@model.spec <- as.formula("~ 1")
md <- try(getModelData(nq), silent = TRUE)
expect_false(inherits(md, "try-error"))
expect_true(inherits(md, "quantmod"))
