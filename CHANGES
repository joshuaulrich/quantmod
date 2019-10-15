0.4-0
MAJOR CHANGES

- getSymbols now uses parent.frame() when auto.assign=TRUE. This
  will cause slightly different behavior than previous versions
  using .GlobalEnv, but is more functional in design.

- getSymbols now allows for env=NULL, which will behave
  as if auto.assign=FALSE is set.

- Upcoming changes for version 0.5-0 will include deprecating
  auto assignment from within getSymbols calls. This will instead
  be moved to the loadSymbols function, to better match get/load
  behaviors in base R.  For the transition, auto.assign will be
  available to force pre 0.5-0 behaviors, but will be discouraged.
  The env= arg will be used for multiple symbol assigns.

0.3-7
- addTA now handles logical vectors or logical xtsible objects
  by drawing bands on chart window

- addTA can now draw on or under any window via 'on=' arg

- chartSeries now cleanly handles series without volume automatically

- addVo has new log.scale option




0.3-6
MODIFICATIONS
- Delt (and functions that call) now defaults
  to 'arithmetic' (discrete) calculations vs. the previous behavior
  of 'log' (continuous) calculation.  This is more inline with
  expected behavior

- 

NEW FUNCTIONALITY
- addTA and newTA allow for dynamic indicator additions with little coding



0.3-2

BUG FIXES
- matched broken TTR calls, aligned arguments between packages

- 'name' of chart was being evaluated somewhere in the process,
  resulting in the object becoming a string. Fixed in this release.

MODIFICATIONS
- continuing the move of time-series functionality to the 'xts' package

- added new TTR functions to addTA.

- added underlay charting to main area (BBands) as well as much
  more advanced shading and labeling.

- chartSeries converts incoming 'x' argument to xts object for
  more universal handling.  Not fully sorted out - but better than
  before.
  
  - new subset argument to allow for xts-style subsetting

NEW FUNCTIONALITY
- new TTR functions - ATR, CCI, CMF, CMO, DPO, Lines, Momentum, TRIX

0.3-1 ***********************************************************************

BUG FIXES
- new depends - on CRAN and R-Forge package xts for time-series handling internally

- options.expiry and futures.expiry now use universal %w to check weekdays

- Rmetrics change resulted in as.timeSeries moving to fSeries.  New suggest and assoc. changes

MODIFICATIONS
- Added ability to plot series with missing values (like those in a 'ts' series)
  Volume with missing obs. is still broken - to be fixed in 0.3-2


0.3-0 **********************************************************************

BUG FIXES
- Fixed factor bug in getSymbols.FRED.  Thanks to Josh Ulrich

- Fixed bug in [.quantmod.OHLC method when i/j was missing,
  also now returns quantmod.OHLC object consistently

MODIFICATIONS
- Added high frequency data handling - to.minutes, to.hourly,
  to.daily.  Additional work done to accomodate within rest of
  framework

- getSymbols downloads now to temp file - instead of directly to
  memory.  Fixed R issue in certain Windows installations 

- getSymbols now returns a character array of symbol names
  written to environment.

- getSymbols includes new arg - auto.assign.  If set to FALSE
  will behave like standard R functions and simply return
  loaded object.  Requires user assignment via '<-'

- Better handling of timeSeries, ts, its within entire package

NEW FUNCTIONALITY

- chartSeries rewrite.  Now manages charting with S4 objects
  stored quietly in memory.  Allowing for dynamic redraws used
  in applying technical indicators and overlays

- addTA functions.  New charting tools to add technicals to
  charts dynamically.  More on the way

- listTA, setTA, unsetTA to handle default TA args

- chartTheme function to customize chart 'look'

- last/first functions now take character strings to describe
  in words the subsetting to do.  Also negative value support
  for opposite behavior.  Additional _keep_ arg will assign
  removed data to an attribute _keep_ with the object

- getSymbols.SQLite support.  Still very clunky - though that is SQLite.

- getFX and getMetals for direct download of those types

- getQuote downloads Last,Change,Open,High,Low,Volume from Yahoo

- added documentation and fixed documentation
