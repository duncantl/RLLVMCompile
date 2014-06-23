foo =
function(x) {
   z = 1L
   z = if(x < 0L)
          x + 2L
       else
          x - 2L

   z + 3L
}

foo.alt =
## Added this because foo is breaking, possible because of lack of `{`
## or return()
function(x) {
   z = 1L
   z = if (x < 0L) {
          x + 2L
        } else {
          x - 2L
        }

   return(z + 3L)
}


bar =
function(x)
{
   z = if(x < 0L) {
          x = x ^2
          x + 2L
       } else if(x > 10L)
          x - 2L
       else {
            x = 2 * x
            x + 1L
       }
         

   z + 3L
}



f = function(n) {
  delta = if(runif(1) > .5) 1 else -1
  delta
}
