ifHandler =
  #
  # Generate code for an if statement
  #
function(call, env, ir, ..., fun = env$.fun, continue = FALSE, nextBlock = NULL)
{
    label = deparse(call)
browser()
         # create blocks for evaluating the condition
         # the body and then where to jump to when we are finished
         # We'll make the nextBlock the location where subsequent
         # instructions are added to continue on from this while() command
#   if(!continue) {
#      cond = Block(fun, sprintf("cond.%s", label))
#      ir$createBr(cond)
#      ir$setInsertPoint(cond)
#   }
    
    bodyBlock = Block(fun, sprintf("body.%s", label))
    if(length(call) == 4) {
           #  we have an else or an else if()
        nextBlock = altBlock = Block(fun, sprintf("else.%s", label))
    } else
      nextBlock = Block(fun, sprintf("next.%s", label))
    
    
#XXX handle series of else if ... here (iteratively) or recursively?
# Need to do it here so we can branch to the correct place

     createConditionCode(call[[2]], env, ir, bodyBlock, nextBlock)

    ir$setInsertPoint(bodyBlock)
    compile(call[[3]], env, ir)
    ir$createBr(nextBlock)

    if(length(call) == 4) {
       #ifHandler(call[[4]], env, ir, ..., continue = TRUE)
       compile(call[[4]], env, ir, continue = TRUE)
    }
     
    ir$setInsertPoint(nextBlock)
}



compile.if = ifHandler =
  #
  # Generate code for an if statement
  #
function(call, env, ir, ..., fun = env$.fun, continue = FALSE, nextBlock = NULL)
{
   # This is not elegant, but brute force.
   # We basically create blocks for each of the if conditions
   # and blocks for each of the bodies, including a trailing else
   # block if present.  It adds a new block at the end which is where
   # we will end up at the conclusion of the if....
   # Then we create the code for each condition and each body
   # knowing where we will branch. If the condition is
   # true, we jump to the body; if it is false, we jump to the next
   # condition block or the trailing else block. If this is not present,
   # we jump to the next block after the if....
  
    label = paste(deparse(call[[2]]), collapse = "")


         # create blocks for evaluating the condition
         # the body and then where to jump to when we are finished
         # We'll make the nextBlock the location where subsequent
         # instructions are added to continue on from this while() command

    bodyBlocks = list()
    condBlocks = list()
    cur = call
    
    while(TRUE) {
         if(is(cur, "if")) {
            tmp = paste(deparse(cur[[2]]), collapse = "")
            condBlocks[[ tmp ]] = Block(fun, sprintf("if.%s", tmp))
            tmp = sprintf("body.%s", tmp)
          } else
            tmp = "body.last"
         bodyBlocks[[tmp]] =  Block(fun, tmp)

         if(!is(cur, "if"))
           break
         cur = cur[[4]]
    }


    nextBlock = Block(fun, sprintf("next.%s", label))
    condBlocks[["final"]] = nextBlock

    ir$createBr(condBlocks[[1]])

    #??? Does this code end up jumping to the trailing else ?
    cur = call
    ctr = 1
    while(TRUE) {
      body = bodyBlocks[[ctr]]
      alt = if(ctr <= length(condBlocks))
               condBlocks[[ctr]]
            else
               nextBlock
      
      if(is(cur, "if")) {
           ir$setInsertPoint(alt)
           createConditionCode(cur[[2]], env, ir, body, condBlocks[[ctr + 1]])
           ir$setInsertPoint(body)
           compile(cur[[3]], env, ir)           
       } else {
           ir$setInsertPoint(body)
           compile(cur, env, ir)                      
       }
      ir$createBr(nextBlock)  # jump to the end of the entire if statement           

      if(!is(cur, "if"))
           break
      cur = cur[[4]]
      ctr = ctr + 1
    }
     
    ir$setInsertPoint(nextBlock)
}
