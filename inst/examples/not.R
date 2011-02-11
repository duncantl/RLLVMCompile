not.fun =
function(x) {
  return(!x)
}

not.fun.alt = 
function(x) {
  if (!x)
    return(1L)
  return(100)
}

not.fun(3)
not.fun(TRUE)
not.fun(as.matrix(3))
