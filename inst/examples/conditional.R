a =
function(x, verbose = TRUE)
{
   if(verbose)
     cat("starting a\n")

   if(length(x) == 0)
     return(numeric())
   
   if(all(x))
     TRUE
   else
      2.0
}
