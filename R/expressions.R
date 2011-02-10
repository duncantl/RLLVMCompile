makeFun =
 # e = quote( { x = 1 + y;  sprintf("0x%f", x)})
function(expr, where = globalenv())
{
  f = function() {}
  body(f) = expr
  library(codetools)
  g = findGlobals(f, merge = FALSE)

  types = sapply(g$var, function(x) getType(get(x, where)))

  cmd = paste("alist(", sprintf("%s = ", g$var), ")")
  formals(f) = eval(parse(text = cmd))
#  for(i in g)
#    formals(f) = alist()

  environment(f) = where
  f
}

getType =
function(val)
{
  "numeric"
}
