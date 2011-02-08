breakHandler =
function(call, env, ir, ...)
{
  if(!exists(".nextBlock", env) || is.null(env$.nextBlock))
     stop("No next block available. Can't break from this code")

  ir$createBr(env$.nextBlock)
}

nextHandler =
function(call, env, ir, ...)
{
  if(!exists(".continueBlock", env) || is.null(env$.continueBlock))
     stop("No continue block available. Can't call next from this code")

  ir$createBr(env$.nextBlock)
}
