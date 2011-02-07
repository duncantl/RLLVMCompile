ifHandler =
  #
  # Generate code for an if statement
  #
function(call, env, ir, ..., fun = env$.fun, continue = FALSE)
{
    label = deparse(call)
browser()
         # create blocks for evaluating the condition
         # the body and then where to jump to when we are finished
         # We'll make the nextBlock the location where subsequent
         # instructions are added to continue on from this while() command
    if(!continue) {
       cond = Block(fun, sprintf("cond.%s", label))
       ir$createBr(cond)
       ir$setInsertPoint(cond)
    }
    
    bodyBlock = Block(fun, sprintf("body.%s", label))
    nb = Block(fun, sprintf("next.%s", label))

    if(length(call) == 4) {
           #  we have an else or an else if()
        nextBlock = altBlock = Block(fun, sprintf("else.%s", label))
    } else
      nextBlock = nb
    
    
#XXX handle series of else if ... here (iteratively) or recursively?
# Need to do it here so we can branch to the correct place

     createConditionCode(call[[2]], env, ir, bodyBlock, nextBlock)

    ir$setInsertPoint(bodyBlock)
    compileExpressions(call[[3]], env, ir)

    if(length(call) == 4) {
       #ifHandler(call[[4]], env, ir, ..., continue = TRUE)
       compile(call[[4]], env, ir, continue = TRUE)
    }
     
    ir$setInsertPoint(nextBlock)
}
