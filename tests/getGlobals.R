f =
    #
    # This is a good example as we see foo used in the default values
    # and defined before x is referenced. So foo should not be a global variable
    # We can extend getGlobals() to handle this, up to conditionals.
    
function(x = foo(globalVar), y = length(x))
{
   y = 1
   foo = function(w)
             w + y
   bar = function()
            y + a + g

   foo(x)
   a = 10
   z = bar()
}


gv = getGlobals(f)
unique(gv$variables)



g =
function(a = x, b = y)
{
   x = 1
   y = 2
   a + b * (x + y)
}

getGlobals(g)$variables

library(CodeAnalysis)
tmp = substituteDefaultValues(g)
formals(tmp)[] = replicate(length(formals(tmp)), formals(getGlobals)[[1]], simplify = FALSE)

getGlobals(tmp)$variables

