f =
function(x)
{
   y = 1
   foo = function(x)
             x + y
   bar = function()
            y + a + g

   foo()
   a = 10
   z = bar()
}


gv = findGlobals(f)
unique(gv$variables)

