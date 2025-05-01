### NOTE
# These tests are not with the rest of the test suite because setDefaults()
# only works with language symbol arguments at the top level. tinytest runs
# all tests in an environment that's different from the Global Environment.

library(quantmod)
options(useFancyQuotes = FALSE)

api.key <- "abc"
src <- "xyz"

# {{{ Unexported function

### function name as character
### --------------------------

## default argument as character
# set
setDefaults("getQuote.av", api.key = "abc")
default.key <- getDefaults("getQuote.av")[["api.key"]]
stopifnot(identical("'abc'", default.key))
# unset
unset <- unsetDefaults("getQuote.av", confirm = FALSE)
stopifnot(!is.null(unset))  # should not be NULL
default.key <- getDefaults("getQuote.av")[["api.key"]]
stopifnot(is.null(default.key))

## default argument as symbol
# set 
setDefaults("getQuote.av", api.key = api.key)
default.key <- getDefaults("getQuote.av")[["api.key"]]
stopifnot(identical(sQuote(api.key), default.key))
# unset
unset <- unsetDefaults("getQuote.av", confirm = FALSE)
stopifnot(!is.null(unset))  # should not be NULL
default.key <- getDefaults("getQuote.av")[["api.key"]]
stopifnot(is.null(default.key))

### function name as symbol
### -----------------------

## default argument as character
# set
setDefaults(getQuote.av, api.key = "abc")
default.key <- getDefaults(getQuote.av)[["api.key"]]
stopifnot(identical("'abc'", default.key))
# unset
unset <- unsetDefaults(getQuote.av, confirm = FALSE)
stopifnot(!is.null(unset))  # should not be NULL
default.key <- getDefaults(getQuote.av)[["api.key"]]
stopifnot(is.null(default.key))

## default argument as symbol
fake.key <- "abc"
# set
setDefaults(getQuote.av, api.key = fake.key)
default.key <- getDefaults(getQuote.av)[["api.key"]]
stopifnot(identical(sQuote(fake.key), default.key))
# unset
unset <- unsetDefaults(getQuote.av, confirm = FALSE)
stopifnot(!is.null(unset))  # should not be NULL
default.key <- getDefaults(getQuote.av)[["api.key"]]
stopifnot(is.null(default.key))

# }}} Unexported function


# {{{ Exported function

### function name as character
### --------------------------

## default argument as character
# set
setDefaults("getSymbols", src = "xyz")
default.src <- getDefaults("getSymbols")[["src"]]
stopifnot(identical("'xyz'", default.src))
# unset
unset <- unsetDefaults("getSymbols", confirm = FALSE)
stopifnot(!is.null(unset))  # should not be NULL
default.src <- getDefaults("getSymbols")[["src"]]
stopifnot(is.null(default.src))

## default argument as symbol
# set
setDefaults("getSymbols", src = src)
default.src <- getDefaults("getSymbols")[["src"]]
stopifnot(identical("'xyz'", default.src))
# unset
unset <- unsetDefaults("getSymbols", confirm = FALSE)
stopifnot(!is.null(unset))  # should not be NULL
default.src <- getDefaults("getSymbols")[["src"]]
stopifnot(is.null(default.src))

### function name as symbol
### -----------------------

## default argument as character
# set
setDefaults(getSymbols, src = "xyz")
default.src <- getDefaults(getSymbols)[["src"]]
stopifnot(identical("'xyz'", default.src))
# unset
unset <- unsetDefaults(getSymbols, confirm = FALSE)
stopifnot(!is.null(unset))  # should not be NULL
default.src <- getDefaults(getSymbols)[["src"]]
stopifnot(is.null(default.src))

## default argument as symbol
# set
setDefaults(getSymbols, src = src)
default.src <- getDefaults(getSymbols)[["src"]]
stopifnot(identical("'xyz'", default.src))
# unset
unset <- unsetDefaults(getSymbols, confirm = FALSE)
stopifnot(!is.null(unset))  # should not be NULL
default.src <- getDefaults(getSymbols)[["src"]]
stopifnot(is.null(default.src))

# }}} Exported function
