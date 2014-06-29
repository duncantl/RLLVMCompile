breakHandler =
function(call, env, ir, ..., .targetType = NULL)
{
  if(!exists(".nextBlock", env) || is.null(env$.nextBlock))
     stop("No next block available. Can't break from this code")

  ir$createBr(env$.nextBlock[[1]])
}

nextHandler =
function(call, env, ir, ..., .targetType = NULL)
{
  if(!exists(".continueBlock", env) || is.null(env$.continueBlock))
     stop("No continue block available. Can't call next from this code")

  ir$createBr(env$.continueBlock[[1]])
}
