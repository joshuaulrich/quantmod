`peak` <-
function(x) {
  which(diff(sign(diff(x, na.pad=FALSE)),na.pad=FALSE) < 0) + 2
}

`valley` <-
function(x) {
  which(diff(sign(diff(x, na.pad=FALSE)),na.pad=FALSE) > 0) + 2
}

