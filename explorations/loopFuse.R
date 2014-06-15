average =
function(x, n = length(x))
{
  total = 0
  for(val in x)
    total = total + val
  total/length(x)
}

stdDev =
function(x, n = length(x))
{
  total.x = 0
  total.sq = 0  
  
  for(val in x) {
    total.x = total.x + val
    total.sq = total.sq + val^2
  }
  sqrt( (total.sq - n * ((total.x/n)^2))/(n - 1))
}


fuse = 
function(..., .funs = list(...))
{
  # get the names from ... by deparse(substitute)

  code = lapply(.funs, body)
  f = function() {}
  tmp = unlist(lapply(code, as.list), recursive = FALSE)
 browser()
  body(f)[seq(along = tmp)] =  tmp
  f
}
