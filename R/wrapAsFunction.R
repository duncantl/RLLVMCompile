wrapAsFunction  =
  #
  # Wrap code into a function and specify the free variables as parameters.
  #
  # e = quote( for(i in 1:B) {
  #     d.star = data[sample(n, n, replace = TRUE), ]
  #     ans[[i]] = T(d.star)
  #  })
  # wrapAsFunction(e)
  #
  # wrapAsFunction(quote({ x = a; y = 2}))
  # wrapAsFunction(quote({ x = 1; y = 2}))
  #
function(code, globalFuns = character(), params = getFreeVars(code, globalFuns),
          env = globalenv())
{
   f = function() {}
   environment(f) = env
   if(is(code, "{"))
      body(f) = code
   else
      body(f)[[2]] = code 
   formals(f) = params
   f
}


getFreeVars =
function(code, globalFuns = character())
{
  fun = function() { 1 }
  if(is(code, "{"))
      body(fun) = code
  else
     body(fun)[[2]] = code
  g = findGlobals(fun, FALSE)
  

  # see if any of the functions referenced are also
  g$variables = c(g$variables, globalFuns)

  if(length(g$variables)) {
    fm = rep(alist(x = ), length(g$variables))
    names(fm) = g$variables    
    formals(fun) = fm
  }
  
  formals(fun)
}
