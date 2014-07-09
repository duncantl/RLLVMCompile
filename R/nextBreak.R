breakHandler =
function(call, env, ir, ..., .targetType = NULL, breakBlock = NULL)
{

  if(!is.null(breakBlock))
     return( ir$createBr(breakBlock) )

  #XXX PROBABLY  wrong now.
  if(!exists(".nextBlock", env) || is.null(env$.nextBlock))
     stop("No next block available. Can't break from this code")

  ir$createBr(env$.nextBlock[[1]])
}

nextHandler =
function(call, env, ir, ..., .targetType = NULL, nextBlock = NULL, nextIterBlock = NULL)
{

  if(!is.null(nextIterBlock))
     return( ir$createBr(nextIterBlock) )
  
  #XXX PROBABLY  wrong now.    
  if(!exists(".continueBlock", env) || is.null(env$.continueBlock))
     stop("No continue block available. Can't call next from this code")

  ir$createBr(env$.continueBlock[[1]])
}
