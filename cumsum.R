Cumsum =
  # This does the computation in line
function(x)
{
   for(i in 2:length(x))  # pardon the case where x is of length 1 or 0
     x[i] = x[i-1] + x[i]

   return(x) # don't need the return
}
