walker =
function()
{

  leaf = function(e, this) {
           browser()
           print(e)
         }

  call = function(e, this) {
             cat('call')
             print(e)
             sapply(e, call, this)
         }

  handler = function(what, this) {
                function(e, this)
                  print(e)
                  cat("In handler\n")
            }
  
    list(handler = handler, call = call, leaf = leaf)
 
}
