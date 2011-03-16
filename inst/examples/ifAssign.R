foo =
function(x)
{
   z = 1L
   z = if(x < 0L)
          x + 2L
       else
          x - 2L

   z + 3L
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
