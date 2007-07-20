"months" <-
function (x, abbreviate) 
{
  UseMethod("months")
}
"quarters" <-
function (x, abbreviate) 
{
  UseMethod("quarters")
}
"weekdays" <-
function (x, abbreviate) 
{
  UseMethod("weekdays")
}
"weeks" <-
function(x, abbreviate)
{
  UseMethod("weeks")
}
"years" <-
function(x, abbreviate)
{ 
  UseMethod("years")
}
"months.zoo" <-
function(x, abbreviate = FALSE)
{
  format(index(x), ifelse(abbreviate, "%m", "%B"))
}
"quarters.zoo" <-
function(x, abbreviate = FALSE)
{
  x <- (as.POSIXlt(index(x))$mon) %/% 3;
  return(x+1);
}

"weekdays.zoo" <-
function(x, abbreviate = FALSE)
{
  format(index(x), ifelse(abbreviate, "%w", "%A"))
}
"weeks.zoo" <-
function(x, abbreviate = FALSE)
{
  format(index(x),"%W")
}
"years.zoo" <-
function(x, abbreviate = FALSE)
{
  format(index(x), ifelse(abbreviate, "%y", "%Y"))
}
