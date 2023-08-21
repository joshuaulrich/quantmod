### Changes in 0.4.25 (2023-08-21)

1. Fix `getQuote.yahoo()` for API changes. Thanks to Ethan B. Smith for the
    report and patch! Also add error message for users in GDPR countries, since
    we cannot automatically consent to GDPR and the request fails without
    consent.
    [#392](https://github.com/joshuaulrich/quantmod/issues/392)
    [#393](https://github.com/joshuaulrich/quantmod/issues/393)
    [#395](https://github.com/joshuaulrich/quantmod/issues/395)

1. Fix `getQuote.yahoo()` when the user only requested metrics that do not have
    have a value for 'regularMarketTime'. Set the value to NA in these cases
    so the output remains the same regardless of whether the endpoint returns
    a 'regularMarketTime' or not. Thanks to @mehdiMBH for the report!
    [#255](https://github.com/joshuaulrich/quantmod/issues/255)

1. Add fields to `getQuote.yahoo()` that are returned when no fields are
    explicitly requested. Thanks to @Courvoisier13 for the report!
    [#335](https://github.com/joshuaulrich/quantmod/issues/335)

1. Add intraday endpoint to `getSymbols.yahoo()`. Thanks to @kapsner for the
    report and patch! Also allow suppressing the warning if more than 7 days
    of data are requested (@eddelbuettel).
    [#351](https://github.com/joshuaulrich/quantmod/issues/351)
    [#381](https://github.com/joshuaulrich/quantmod/issues/381)
    [#399](https://github.com/joshuaulrich/quantmod/issues/399)

1. Add warning if `getSymbols()` is called with tickers that are reserved words
    because accessing them requires back-quotes (e.g. ``NA``).
    [#401](https://github.com/joshuaulrich/quantmod/issues/401)

1. Fix `allReturns()` when 'subset' is specified. Thanks to @Panagis1980 for
    the report!
    [#402](https://github.com/joshuaulrich/quantmod/issues/402)

### Changes in 0.4.24 (2023-07-17)

1. Fix `getSymbols.oanda()` URL. Thanks to @macray76 for the report.
    [#387](https://github.com/joshuaulrich/quantmod/issues/387)

### Changes in 0.4.23 (2023-06-14)

1. Fix `getQuote.yahoo()` error. Thanks to Ethan B. Smith for the report and
    patch!
    [#382](https://github.com/joshuaulrich/quantmod/issues/382)
    [#383](https://github.com/joshuaulrich/quantmod/issues/383)

1. Add `name` argument to `add_TA()`. Thanks to @SamoPP for the suggestion!
    [#377](https://github.com/joshuaulrich/quantmod/issues/377)
    [#205](https://github.com/joshuaulrich/quantmod/issues/205)

### Changes in 0.4.22 (2023-04-05)

1. Move jsonlite from Suggests to Imports so it doesn't cause a problem
    when a package that doesn't also Suggest jsonlite uses getSymbols().
    Thanks to Kurt Hornik for the report and fix!
    [#380](https://github.com/joshuaulrich/quantmod/issues/380)

### Changes in 0.4.21 (2023-03-29)

1. Fix S3 method issues. R-devel (83995-ish) added a check for possible S3
    method issues. Register methods it found that were not registered:
    `str.replot()`, `seriesHi.timeSeries()`, and `seriesLo.timeSeries()`.

    It was also confused by `range.bars()` and `unique.formula.names()`. Remove
    `unique.formula.names()` because it wasn't exported or used internally.
    Rename `range.bars()` to `rangeBars()`, which isn't exported.

    Thanks to Kurt Hornik for the report!
    [#375](https://github.com/joshuaulrich/quantmod/issues/375)

1. Remove "^" prefix from `getSymbols()` return value. When the 'Symbols'
    argument has a "^" prefix and `auto.assign = TRUE`:

    * `getSymbols()` removes the "^" from the object it creates, but
    * returns the 'Symbols' argument unchanged, and
    * removes the "^" from the column names of the object it creates.

    The example below will create an object named `IXIC` but the value of
    `sym` will be "^IXIC".

        sym <- getSymbols("^IXIC")

    That means `x <- get(sym)` will not work because an object named `^IXIC`
    doesn't exist.
    [#371](https://github.com/joshuaulrich/quantmod/issues/371)

1. Add 'from' and 'to' arguments to `getSymbols.FRED()`. Users expect to be
    able to set the 'from' and 'to' arguments for FRED data like they can for
    Yahoo data. Those values were ignored and the entire series was always
    returned.
    [#368](https://github.com/joshuaulrich/quantmod/issues/368)

1. Change interval to 1d for `getDividends()` and `getSplits()`. The "3mo"
    setting caused some dividends to be missing for companies that issued monthly
    dividends. Note that the response to this request also includes all the OHLCV
    data. But it's small (less than 1MB for 60+ years of daily data).
    [#372](https://github.com/joshuaulrich/quantmod/issues/372)

1. Handle errors in `getSplits()` and `getDividends()`. `getDividends()` didn't
    handle cases where the download failed, or when dividends needed to be
    split-adjusted but there were no splits. It also tried to set colnames
    on the empty xts object that's returned when there are no dividends.
    `getSplits()` had the same colnames issue. Check for no splits by testing
    for `NULL` because that's more explicit. Thanks to Chris Cheung for the
    report!
    [#366](https://github.com/joshuaulrich/quantmod/issues/366)

1. Export `HL()`, `is.HL()`, and `has.HL()` functions and add documentation.
    These were added in 0.4.18 but not exported or included in the documentation.

1. Use Yahoo Finance v8 JSON endpoint and remove the v7 CSV endpoint. There
    seems to be a rate limit for the number of tickers you can request via the CSV
    endpoint. The [yfinance python library](https://github.com/ranaroussi/yfinance)
    uses the JSON endpoint and doesn't seem to have rate limit issues.
    [#360](https://github.com/joshuaulrich/quantmod/issues/360)
    [#362](https://github.com/joshuaulrich/quantmod/issues/362)
    [#364](https://github.com/joshuaulrich/quantmod/issues/364)

### Changes in 0.4.20 (2022-04-29)

1. Remove check for Yahoo Finance cookies because the site no longer
  responds with a cookie, and that caused the connection attempt to fail.
  This affected `getSymbols()`, `getDividends()`, and `getSplits()`.
  Thanks to several users for reporting, and especially to @pverspeelt and
  @alihru for investigating potential fixes!
  [#358](https://github.com/joshuaulrich/quantmod/issues/358)

1. Update `getSymbols.yahooj()` for changes to the web page.
  [#312](https://github.com/joshuaulrich/quantmod/issues/312)

1. Add `HL()` and supporting functions. These are analogues to `HLC()`,
  `OHLC()`, etc. Thanks for Karl Gauvin for the nudge to implement them.

1. Add adjusted close to `getSymbols.tiingo()` output. Thanks to Ethan Smith
  for the suggestion and patch!
  [#289](https://github.com/joshuaulrich/quantmod/issues/289)
  [#345](https://github.com/joshuaulrich/quantmod/pull/345)

1. Use a Date index for `getSymbols.tiingo()` daily data. Thanks to Ethan
  Smith for the report!
  [#350](https://github.com/joshuaulrich/quantmod/issues/350)

1. Remove unneeded arguments to the `getSymbols.tiingo()` implementation.
  Thanks to Ethan Smith for the suggestion and patch!
  [#343](https://github.com/joshuaulrich/quantmod/issues/343)
  [#344](https://github.com/joshuaulrich/quantmod/pull/344)

1. Load dividends and splits data into the correct environment when the user
  provides a value for the `env` argument. The previous behavior always loaded
  the data into the environment the function was called from. Thanks to
  Stewart Wright for the report and patch!
  [#33](https://github.com/joshuaulrich/quantmod/issues/33)

1. Make `getOptionChain()` return all the fields that Yahoo Finance provides.
  Thanks to Adam Childers (@rhizomatican) for the patch!
  [#318](https://github.com/joshuaulrich/quantmod/issues/318)
  [#336](https://github.com/joshuaulrich/quantmod/pull/336)

1. Add [orats](https://docs.orats.io) as a source for `getOptionChain()`.
  Thanks to Steve Bronder (@SteveBronder) for the suggestion and implementation!
  [#325](https://github.com/joshuaulrich/quantmod/pull/325)

1. Improve the error message when `getSymbols()` cannot import data for a
  symbol because the symbol is not valid or does not have historical data.
  Thanks to Peter Carl for the report.
  [#333](https://github.com/joshuaulrich/quantmod/issues/333)

1. Fix the `getMetals()` example in the documentation. The example section
  previously had an example of `getFX()`. Thanks to Gerhard Nachtmann
  (@nachti) for the report and patch!
  [#330](https://github.com/joshuaulrich/quantmod/issues/330)

1. Fix `getQuote()` so it returns data when the ticker symbol contains an "&".
  Thanks to @pankaj3009 for the report!
  [#324](https://github.com/joshuaulrich/quantmod/issues/324)

1. Fix `addMACD()` when `col` is specified. Thanks to @nvalueanalytics for the
  report!
  [#321](https://github.com/joshuaulrich/quantmod/issues/321)

### Changes in 0.4.18 (2020-11-29)

1. Fix issues handling https:// in `getSymbols.yahooj()`. Thanks to @lobo1981
  and @tchevri for the reports and @ethanbsmith for the suggestion to move
  from XML to xml2.
  [#310](https://github.com/joshuaulrich/quantmod/issues/310)
  [#312](https://github.com/joshuaulrich/quantmod/issues/312)

1. Fix `getSymbols.yahoo()`, `getDividends()`, and `getSplits()` so they all
  handle download errors and retry again. Thanks for @helgasoft for the report
  on `getSymbols.yahoo()` and @msfsalla for the report on `getDividends()` and
  `getSplits()`.
  [#307](https://github.com/joshuaulrich/quantmod/issues/307)
  [#314](https://github.com/joshuaulrich/quantmod/issues/314)

1. Add implied volatility and last trade date to `getOptionChain()` output.
  Thanks to @hd2581 and @romanlelek for the reports. And thanks to
  @rjvelasquezm for noticing the error when `lastTradeDate` is `NULL`.
  [#224](https://github.com/joshuaulrich/quantmod/issues/224)
  [#304](https://github.com/joshuaulrich/quantmod/issues/304)

1. Fix `getOptionChain()` to throw a warning and return `NULL` for every
  expiry that doesn't have data.
  [#299](https://github.com/joshuaulrich/quantmod/issues/299)

1. Add "Defaults" handling to `getQuote()` and `getQuote.yahoo()`. Thanks to
  @ethanbsmith for the report.
  [#291](https://github.com/joshuaulrich/quantmod/issues/291)

1. Add Bid and Ask fields to the output from `getQuote()`. Thanks to @jrburl
  for the report and PR.
  [#302](https://github.com/joshuaulrich/quantmod/pull/302)

1. Fix "Defaults" to handle unexported function (e.g. `getQuote.av()`. Thanks
  to @helgasoft for the report.
  [#316](https://github.com/joshuaulrich/quantmod/issues/316)

1. `importDefaults()` doesn't call `get()` on vector with length > 1. Thanks
  to Kurt Hornik for the report.
  [#319](https://github.com/joshuaulrich/quantmod/issues/319)

### Changes in 0.4.17 (2020-03-31)

1. `chartTheme()` now works when quantmod is not attached. Thanks to Kurt
  Hornik for the report.

### Changes in 0.4-16 (2020-03-08)

1. Remove disk I/O from `getSymbols()` and `getQuote()`. This avoids any disk
  contention, and makes the implementation pattern more consistent with other
  functions that import data. Thanks to Ethan Smith suggestion and PR.
  [#280](https://github.com/joshuaulrich/quantmod/issues/280)
  [#281](https://github.com/joshuaulrich/quantmod/pull/281)

1. Make `getQuote()` robust to symbols without data, so it does not error if
  one or more symbols are not found. Also return quotes in the same order as
  the 'Symbols' argument. Thanks to Ethan Smith feature request and PR.
  [#279](https://github.com/joshuaulrich/quantmod/issues/279)
  [#282](https://github.com/joshuaulrich/quantmod/pull/282)
  [#288](https://github.com/joshuaulrich/quantmod/pull/288)

1. Handle semicolon-delimited symbol string handling to main `getQuote()`
  function. This makes `getQuote()` consistent with `getSymbols()`. Thanks to
  Ethan Smith suggestion and PR.
  [#284](https://github.com/joshuaulrich/quantmod/issues/284)
  [#285](https://github.com/joshuaulrich/quantmod/pull/285)

1. Fix ex-dividend and pay date mapping. `getQuote()` returned the dividend
  pay date labeled as the ex-dividend date. Thanks to @matiasandina for the
  report.
  [#287](https://github.com/joshuaulrich/quantmod/issues/287)

1. Fix Yahoo Finance split ratio. The delimiter changed from "/" to ":".
  For example, a 2-for-1 split was 1/2 but is now "2:1". Thanks to @helgasoft
  for the report.
  [#292](https://github.com/joshuaulrich/quantmod/issues/292)

1. Error messages from `getQuote.alphavantage()` and `getQuote.tiingo()` no
  longer contain the API key when symbols can't be found.
  [#286](https://github.com/joshuaulrich/quantmod/issues/286)

1. Fix `getQuote.alphavantage()` by replacing the defunct batch quote request
  with a loop over the single quote request. Thanks to @helgasoft for the
  report and patch.
  [#296](https://github.com/joshuaulrich/quantmod/issues/296)

1. Update `getOptionChain()` to handle empty volume or open interest
  Thank to @jrburl for the report and PR.
  [#299](https://github.com/joshuaulrich/quantmod/issues/299)
  [#300](https://github.com/joshuaulrich/quantmod/pull/300)


### Changes in 0.4-15 (2019-06-15)

1. Add an environment variable to control whether to run tests that import
  from Yahoo Finance. `getDividends()` tests were failing because Yahoo
  Finance wasn't returning all dividend history for "CF".
1. Write one message the first time `quantmod::as.zoo.data.frame()` is called.
  This method was added years before `zoo::as.zoo.data.frame()` existed, but
  it should be deprecated in favor of the zoo version. The package that owns
  the class should also own the methods.

### Changes in 0.4-14 (2019-03-23)

#### BUG FIXES
1. Fix `getSymbols.tiingo()` so the Open and Close columns aren't swapped.
  Thanks to Steve Bronder for the report and PR.
  [#233](https://github.com/joshuaulrich/quantmod/pull/233)
  [#234](https://github.com/joshuaulrich/quantmod/issues/234)
1. Fix `getQuote.yahoo()` for quotes in multiple timezones. Thanks to
  Philippe Verspeelt for the report and PR.
  [#246](https://github.com/joshuaulrich/quantmod/issues/246)
  [#248](https://github.com/joshuaulrich/quantmod/pull/248)
1. Update `getDividends()` because Yahoo Finance now provides raw dividends
  instead of split-adjusted dividends. Thanks to Douglas Barnard for the
  report.
  [#253](https://github.com/joshuaulrich/quantmod/issues/253)
1. Fix `futures.expiry()`. Thanks to @pjheink for the report.
  [#257](https://github.com/joshuaulrich/quantmod/issues/257)
1. Fix `getSymbols.tiingo()` to return correct columns for ticker "LOW".
  Thanks to @srtg4we5gsetrgwhreyt the report.
  [#259](https://github.com/joshuaulrich/quantmod/issues/259)
1. Fix `getSymbols.yahooj()` to avoid infinite loop when the requested
  symbol doesn't have data. Thanks to Wouter Thielen for the review.
  [#63](https://github.com/joshuaulrich/quantmod/issues/63)
1. Update `getSplits()` because Yahoo Finance now provides the actual split
  adjustment ratio, instead of the inverse (e.g. now 1/2 instead of 2/1).
  [#265](https://github.com/joshuaulrich/quantmod/issues/265)


#### NEW FEATURES
1. Extend `getQuote()` to support Tiingo. Thanks to Ethan Smith for the
  feature request and PR.
  [#247](https://github.com/joshuaulrich/quantmod/issues/247)
  [#250](https://github.com/joshuaulrich/quantmod/pull/250)
1. Extend `getSymbols()` to catch errors for individual ticker symbols and
  continue processing any remaining ticker symbols, instead of throwing an
  error. More useful error messages are also provided. Thanks to @helgasoft
  for testing and feedback.
  [#135](https://github.com/joshuaulrich/quantmod/issues/135)


### Changes in 0.4-13 (2018-04-13)

#### BUG FIXES
1. Fix `getQuote.yahoo()` when a field has no data for all requested tickers.
  [#208](https://github.com/joshuaulrich/quantmod/issues/208)
1. Expose weekly and monthly adjusted prices from Alpha Vantage's API.
  [#212](https://github.com/joshuaulrich/quantmod/issues/212)
1. Fix `saveChart()` (it actually saves a chart now!).
  [#154](https://github.com/joshuaulrich/quantmod/issues/154)
1. Update Oanda URL, which fixes `getSymbols.oanda()` and `getFX()`.
  [#225](https://github.com/joshuaulrich/quantmod/issues/225)

#### NEW FEATURES
1. Add `getQuote.alphavantage()`, thanks to Ethan Smith for the PR.
  [#213](https://github.com/joshuaulrich/quantmod/issues/213)
  [#223](https://github.com/joshuaulrich/quantmod/issues/223)
1. Add `getSymbols.tiingo()` to import data from [Tiingo](https://www.tiingo.com/).
  Thanks to Steve Bronder for the PR.
  [#220](https://github.com/joshuaulrich/quantmod/issues/220)

#### BREAKING CHANGES
1. Google Finance no longer provides data for historical prices or financial
   statements, so all Google data sources are defunct.
  [#221](https://github.com/joshuaulrich/quantmod/issues/221)


### Changes in 0.4-12 (2017-12-02)

#### BUG FIXES
1. `chartSeries()` now honors `show.grid` argument.  Thanks to Ethan Smith.
  [#200](https://github.com/joshuaulrich/quantmod/issues/200)
1. `getQuote.yahoo()` uses the new JSON API.
  [#197](https://github.com/joshuaulrich/quantmod/issues/197)
1. `getSymbols.yahoo()` is more careful about converting UNIX timestamps to
  character when creating the query URL.
  [#202](https://github.com/joshuaulrich/quantmod/issues/202)


### Changes in 0.4-11 (2017-10-06)

#### BUG FIXES
1. `getSymbols.yahoo()`
    * Don't try to un-adjust the OHLC for splits and/or dividends. Return data
      as-is and leave any (un-)adjustments to the end user.
      [#174](https://github.com/joshuaulrich/quantmod/issues/174)
    * Add ability to pass `curl.options` to `curl.download()`.
      [#177](https://github.com/joshuaulrich/quantmod/issues/177)

#### NEW FEATURES
1. `getSymbols.av()` can download data from [Alpha Vantage](https://www.alphavantage.co/).
  Thanks to Paul Teetor for the contribution.
  [#176](https://github.com/joshuaulrich/quantmod/issues/176)


### Changes in 0.4-10 (2017-06-20)

#### BUG FIXES
1. `getSymbols.yahoo()`
    * Avoid cached response from Yahoo Finance proxy.
      [#166](https://github.com/joshuaulrich/quantmod/issues/166)
    * Set `from` argument back to 1900-01-01.
      [#157](https://github.com/joshuaulrich/quantmod/issues/157)
1. `getSymbols()` no longer warns if called with namespace
  (i.e. `quantmod::getSymbols()`).
  [#134](https://github.com/joshuaulrich/quantmod/issues/134)
1. `as.zoo.data.frame()` now ignores `row.date` argument if called with
  `order.by`.
  [#168](https://github.com/joshuaulrich/quantmod/issues/168)


### Changes in 0.4-9 (2017-05-29)

#### BUG FIXES
1. `getSymbols.yahoo()` uses the new API.
  [#157](https://github.com/joshuaulrich/quantmod/issues/157)
1. `getOptionChain.yahoo()` returns `NULL` when there are no calls/puts instead
  of `list()`.
  [#155](https://github.com/joshuaulrich/quantmod/issues/155)

#### NEW FEATURES
1. `getSymbols.yahoo()` gains a `periodicity` argument, for use by
  `tseries::get.hist.quote()`.
  [#162](https://github.com/joshuaulrich/quantmod/issues/162)


### Changes in 0.4-8 (2017-04-19)

#### BUG FIXES
1. `getSymbols.google()`:
    * Honor all arguments set via `setSymbolLookup()`.
    * Correctly parse dates in non-English locales.
1. Fix `getSymbols.oanda()`.
1. Fix `add_TA()` when called from a function.
1. Remove 'its' package references (it was archived).
1. Update Yahoo Finance URLs to HTTPS to avoid redirect.
1. Update FRED URL to avoid redirect.

#### NEW FEATURES
1. Add `split.adjust` argument to `getDividends()`.
1. Add readme, contributing, and issue template files for GitHub.


### Changes in 0.4-7 (2016-10-24)

1. Let `jsonlite::fromJSON()` manage connections in `getOptionChain.yahoo()`.
1. Update omegahat URL at CRAN's request.


### Changes in 0.4-6 (2016-08-28)

1. Remove unused `unsetSymbolLookup()`.
1. Add documentation for `getPrice()`.
1. Fix subsetting in `addTRIX()`.
1. Fix `getSymbols.oanda()` to use https.
1. Fix `getOptionChain.yahoo()` to download JSON instead of scrape HTML.


### Changes in 0.4-5 (2015-07-24)

1. Ensure `add*MA()` functions use Close column by default.
1. Correct `Delt()` docs (type argument default value was wrong).
1. Ensure tempfiles are always removed.
1. In `getSymbols.csv()`:
    * Fix format argument handling.
    * Ensure date column is character before calling `as.Date()`.
    * Add `col.names` argument.
1. Fix `dbConnect()` call (changed in `RMySQL_0.10`) in `getSymbols.MySQL()`.
1. Automatically detect OHLC vs OHLCVA in `getSymbols.yahooj()`.
1. Handle long vectors in `setDefaults()`.
1. Fix `getSymbols.FRED() for https.
1. Fix `getOptionChain.yahoo() for spaces in table headers.
1. Add `importFrom` for all non-base packages.


### Changes in 0.4-4 (2015-03-08)

1. Added `getSymbols.yahooj()` to pull data from Yahoo Finance Japan (Thanks to
  Wouter Thielen for the contribution.
  [#14](https://github.com/joshuaulrich/quantmod/issues/14)).
1. Fixed `getOptionChain.yahoo()` to handle the new options page layout.
  [#27](https://github.com/joshuaulrich/quantmod/issues/27)
1. Fixed `getSymbols.oanda()` to handle the new URL structure and CSV format.
  [#36](https://github.com/joshuaulrich/quantmod/issues/36)


### Changes in 0.4-3 (2014-12-15)

1. Change maintainer from Jeffrey Ryan to Joshua Ulrich

1. Copy required functionality from the (archived) Defaults package into
   quantmod and remove dependency on Defaults.

1. Incorporate several bug fixes and patches.


### Changes in 0.4-0

* getSymbols now uses parent.frame() when auto.assign=TRUE. This
  will cause slightly different behavior than previous versions
  using .GlobalEnv, but is more functional in design.

* getSymbols now allows for env=NULL, which will behave
  as if auto.assign=FALSE is set.

* Upcoming changes for version 0.5-0 will include deprecating
  auto assignment from within getSymbols calls. This will instead
  be moved to the loadSymbols function, to better match get/load
  behaviors in base R.  For the transition, auto.assign will be
  available to force pre 0.5-0 behaviors, but will be discouraged.
  The env= arg will be used for multiple symbol assigns.

### Changes in 0.3-7
* addTA now handles logical vectors or logical xtsible objects
  by drawing bands on chart window

* addTA can now draw on or under any window via 'on=' arg

* chartSeries now cleanly handles series without volume automatically

* addVo has new log.scale option


### Changes in 0.3-6

#### MODIFICATIONS
* Delt (and functions that call) now defaults
  to 'arithmetic' (discrete) calculations vs. the previous behavior
  of 'log' (continuous) calculation.  This is more inline with
  expected behavior

#### NEW FUNCTIONALITY
* addTA and newTA allow for dynamic indicator additions with little coding


### Changes in 0.3-2

#### BUG FIXES
* matched broken TTR calls, aligned arguments between packages

* 'name' of chart was being evaluated somewhere in the process,
  resulting in the object becoming a string. Fixed in this release.

#### MODIFICATIONS
* continuing the move of time-series functionality to the 'xts' package

* added new TTR functions to addTA.

* added underlay charting to main area (BBands) as well as much
  more advanced shading and labeling.

* chartSeries converts incoming 'x' argument to xts object for
  more universal handling.  Not fully sorted out - but better than
  before.
  
* new subset argument to allow for xts-style subsetting

#### NEW FUNCTIONALITY
* new TTR functions - ATR, CCI, CMF, CMO, DPO, Lines, Momentum, TRIX

### Changes in 0.3-1

#### BUG FIXES
* new depends - on CRAN and R-Forge package xts for time-series handling internally

* options.expiry and futures.expiry now use universal %w to check weekdays

* Rmetrics change resulted in as.timeSeries moving to fSeries.  New suggest and assoc. changes

#### MODIFICATIONS
* Added ability to plot series with missing values (like those in a 'ts' series)
  Volume with missing obs. is still broken - to be fixed in 0.3-2


### Changes in 0.3-0

#### BUG FIXES
* Fixed factor bug in getSymbols.FRED.  Thanks to Josh Ulrich

* Fixed bug in [.quantmod.OHLC method when i/j was missing,
  also now returns quantmod.OHLC object consistently

#### MODIFICATIONS
* Added high frequency data handling - to.minutes, to.hourly,
  to.daily.  Additional work done to accomodate within rest of
  framework

* getSymbols downloads now to temp file - instead of directly to
  memory.  Fixed R issue in certain Windows installations 

* getSymbols now returns a character array of symbol names
  written to environment.

* getSymbols includes new arg - auto.assign.  If set to FALSE
  will behave like standard R functions and simply return
  loaded object.  Requires user assignment via '<-'

* Better handling of timeSeries, ts, its within entire package

#### NEW FUNCTIONALITY

* chartSeries rewrite.  Now manages charting with S4 objects
  stored quietly in memory.  Allowing for dynamic redraws used
  in applying technical indicators and overlays

* addTA functions.  New charting tools to add technicals to
  charts dynamically.  More on the way

* listTA, setTA, unsetTA to handle default TA args

* chartTheme function to customize chart 'look'

* last/first functions now take character strings to describe
  in words the subsetting to do.  Also negative value support
  for opposite behavior.  Additional _keep_ arg will assign
  removed data to an attribute _keep_ with the object

* getSymbols.SQLite support.  Still very clunky - though that is SQLite.

* getFX and getMetals for direct download of those types

* getQuote downloads Last,Change,Open,High,Low,Volume from Yahoo

* added documentation and fixed documentation
