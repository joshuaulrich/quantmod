findPeaks <-
function(x, thresh=0) {
  pks <- which(diff(sign(diff(x, na.pad=FALSE)),na.pad=FALSE) < 0) + 2
  if( !missing(thresh) ) {
   pks[x[pks-1]-x[pks] > thresh]
  } else pks
}

peak <- function(x) {
  .Deprecated("findPeaks", package="quantmod")
  findPeaks(x)
}

findValleys <-
function(x, thresh=0) {
  pks <- which(diff(sign(diff(x, na.pad=FALSE)),na.pad=FALSE) > 0) + 2
  if( !missing(thresh) ) {
   pks[x[pks-1]-x[pks] > thresh]
  } else pks
}

valley <- function(x) {
  .Deprecated("findValleys", package="quantmod")
  findValleys(x)
}
