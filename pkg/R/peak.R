`peak` <-
function(x) {
  which(diff(sign(diff(x))) < 0) + 2
}

`valley` <-
function(x) {
  which(diff(sign(diff(x))) > 0) + 2
}

