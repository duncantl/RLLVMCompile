#
# fun = compileFunction(noreturn, Int32Type, list(Int32Type))
#

noreturn =
function(x)
{
   x + 1L
}

noreturn1 =
function(x)
{
   if(x > 10)
      x + 1L
   else
      x - 1L
}
