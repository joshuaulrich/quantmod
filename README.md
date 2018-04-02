### About

[quantmod](http://www.quantmod.com) is an [R](https://www.r-project.org)
package that provides a framework for quantitative financial modeling and
trading. It provides a rapid prototyping environment that makes modeling easier
by removing the repetitive workflow issues surrounding data management and
visualization.

### Installation

The current release is available on [CRAN](https://CRAN.R-project.org/package=quantmod),
which you can install via:

```r
install.packages("quantmod")
```

To install the development version, you need to clone the repository and build
from source, or run one of:

```r
# lightweight
remotes::install_github("joshuaulrich/quantmod")
# or
devtools::install_github("joshuaulrich/quantmod")
```

You may need tools to compile C, C++, or Fortran code. See the relevant
appendix in the [R Installation and Administration manual](https://cran.r-project.org/doc/manuals/r-release/R-admin.html)
for your operating system:

- [Windows](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#The-Windows-toolset)
- [MacOS](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#macOS) (the [R for Mac OS X Developer's Page](https://r.research.att.com/) might also be helpful)
- [Unix-alike](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Essential-and-useful-other-programs-under-a-Unix_002dalike)

### Getting Started

It is possible to import data from a variety of sources with one quantmod
function: `getSymbols()`. For example:

```r
> getSymbols("AAPL", src = "yahoo")    # from yahoo finance
[1] "AAPL"
> getSymbols("DEXJPUS", src = "FRED")  # FX rates from FRED
[1] "DEXJPUS"
```

Once you've imported the data, you can use `chartSeries()` to visualize it and
even add technical indicators from the [TTR](https://CRAN.R-project.org/package=TTR)
package:

```r
> getSymbols("AAPL")
[1] "AAPL"
> chartSeries(AAPL)
> addMACD()
> addBBands()
```

###### Have a question?

Ask your question on [Stack Overflow](http://stackoverflow.com/questions/tagged/r)
or the [R-SIG-Finance](https://stat.ethz.ch/mailman/listinfo/r-sig-finance)
mailing list (you must subscribe to post).

###### Want hands-on experience?

- [DataCamp course on importing and managing financial data](https://www.datacamp.com/courses/importing-and-managing-financial-data-in-r)
- [DataCamp course on manipulating time series with xts & zoo](https://www.datacamp.com/courses/manipulating-time-series-data-in-r-with-xts-zoo)

### Contributing

Please see the [contributing guide](CONTRIBUTING.md).

### See Also

- [TTR](https://CRAN.R-project.org/package=TTR): functions for technical trading
rules
- [xts](https://CRAN.R-project.org/package=xts): eXtensible Time Series based
on [zoo](https://CRAN.R-project.org/package=zoo)

### Author

Jeffrey Ryan, [Joshua Ulrich](https://about.me/joshuaulrich)

