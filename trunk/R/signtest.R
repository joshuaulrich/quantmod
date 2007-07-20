"tR.signtest" <-
function(x,mu=0,p=.5) {
  D <- length( (x-mu)[sign(x-mu) > 0] )
  p.value <- sum(dbinom(D:length(x),length(x),p))
  return(list("d-positive" = D,"p.value"=p.value))
}

