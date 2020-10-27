`quantmodVersion` <- function() {
  return(list(Version='0.3-7', Revision=433))
}

`quantmodNews` <- function() {

}

`quantmodChanges` <- function() {

}

`quantmodBugs` <- function() {

}

`quantmodComment` <- function() {

}

`quantmod.com` <- function() {
  browseURL('http://www.quantmod.com')
}

`try.download.file` <-
function(url, destfile, method, quiet = FALSE, mode = "w", cacheOK = TRUE,
         extra = getOption("download.file.extra"), ...)
{
  # no longer used
  # appears to have only been callled by getSymbols.FRED() to handle https
  # downloads that are now handled by curl
  # leaving in place in case needed for some other scenario

  if (missing(method))
    method <- getOption("download.file.method", default="auto")

  # capture download.file errors (e.g. https not supported)
  try.download <- try({
    download.file(url, destfile, method, quiet, mode, cacheOK, extra)
  }, silent=TRUE)

  if (inherits(try.download, "try-error")) {
    if (requireNamespace("downloader", quietly=TRUE)) {
      # use downloader::download, if available
      # everything except 'url' is passed via '...', so name them; and
      # download automatically determines 'method' and errors if supplied
      # as an argument, so omit it
      downloader::download(url, destfile=destfile, quiet=quiet,
                           mode=mode, cacheOK=cacheOK, extra=extra)
    } else {
      # report original error, and provide recommendations
      errcond <- attr(try.download, "condition")
      stop("Failed to download file. Error message:\n", errcond$message, "\n",
           "If this is related to https, possible solutions are:\n",
           "1. Explicitly pass method= via the getSymbols call (or via setDefaults)\n",
           "2. Install downloader, which may be able to automagically determine a method\n",
           "3. Set the download.file.method global option", call.=FALSE)
    }
  }
}

retry.yahoo <-
function(symbol,
         from,
         to,
         interval,
         type,
         conn,
         ...,
         curl.options = list())
{
     warning(symbol, " download failed; trying again.",
             call. = FALSE, immediate. = TRUE)

     # re-create handle
     handle <- .getHandle(curl.options, force.new = TRUE)

     # try again. must rebuild url with crumbs
     yahoo.URL <- .yahooURL(symbol, from, to, interval, type, handle)

     close(conn)
     conn <- curl::curl(yahoo.URL, handle = handle$ch)

     fr <- try(read.csv(conn, ..., as.is = TRUE), silent = TRUE)

     # error if second attempt also failed
     if (inherits(fr, "try-error")) {
         close(conn)
         stop(symbol, " download failed after two attempts. Error",
              " message:\n", attr(fr, "condition")$message, call. = FALSE)
     }

     # return data
     return(fr)
}
