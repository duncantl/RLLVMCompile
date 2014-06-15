makeFun =
 # e = quote( { x = 1 + y;  sprintf("0x%f", x)})
function(expr, where = globalenv())
{
  f = function() {}
  body(f) = expr
  g = findGlobals(f, merge = FALSE)

  types = sapply(g$var, function(x) getDataType(get(x, where)))

  cmd = paste("alist(", sprintf("%s = ", g$var), ")")
  formals(f) = eval(parse(text = cmd))
#  for(i in g)
#    formals(f) = alist()

  environment(f) = where
  f
}

if(FALSE)
 xx.getType =
  function(val)
     "numeric"

