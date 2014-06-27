traverseExpressions =
    #
    # Change calls (and currently only calls)
    #
function(x, fun = function(x) x, ...)
{
   if(is(x, "function")) {
      formals(x) = lapply(formals(x), fun)
      body(x) = traverseExpressions(body(x), fun, ...)
      x
   } else if(is(x, "call")) {
#       x[] = lapply(x, fun)
#       x
       fun(x, ...)
   } else if(is(x, "for")) {
        x[[2]] = fun(x[[2]])
        x[[3]] = traverseExpressions(x[[3]], fun, ...)
        x[[4]] = traverseExpressions(x[[4]], fun, ...)
        x
   } else if(is(x, "if")) {
        x[[2]] = fun(x[[2]])
        x[[3]] = traverseExpressions(x[[3]], fun, ...)
        if(length(x) == 4)
            x[[4]] = traverseExpressions(x[[4]], fun, ...)
        x
   } else if(is(x, "{")) {
       x[2:length(x)] = lapply(x[2:length(x)], traverseExpressions, fun, ...)
       x
   } else if(is(x, "<-") || is(x, "=")) {
       x[2:3] = lapply(x[2:3], traverseExpressions, fun, ...)
       x
   } else
       x
#     UseMethod("traverseExpressions")
}

rewriteRNGCalls =
function(x, ...)
{
#    print(x)
 if(is(x, "call") && as.character(x[[1]]) %in% c("rgamma", "rnorm", "runif") && x[[2]] == 1) {

     x = x[-2]

     if(as.character(x[[1]]) == "rgamma")
            # Rf_rgamma is called by change the rate to scale and passing scale. So use 1/rate
         x[[3]] = substitute(1/(x), list(x = x[[3]]))

     x[[1]] = as.name(sprintf("Rf_%s", as.character(x[[1]])))
         
     i = seq(along = x)[-1]     
     x[i] = lapply(x[i], traverseExpressions, rewriteRNGCalls, ...)
  }

  x
}
