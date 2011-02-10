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

    hasTrailingElse = FALSE
    while(TRUE) {
         if(is(cur, "if")) {
            tmp = paste(deparse(cur[[2]]), collapse = "")
            condBlocks[[ tmp ]] = Block(fun, sprintf("if.%s", tmp))
            tmp = sprintf("body.%s", tmp)
          } else
            tmp = "body.last"
         bodyBlocks[[tmp]] =  Block(fun, tmp)

         if(!is(cur, "if")) {
           hasTrailingElse = TRUE
           break
         }

         if(length(cur) == 4)
            cur = cur[[4]]
         else
           break
    }

      # This is the block that all branches of the if statement
      # will end up in. This is where start the code for the next statement
      # after the if statement.
    nextBlock = Block(fun, sprintf("next.%s", label))
    condBlocks[["final"]] = if(hasTrailingElse) bodyBlocks[[length(bodyBlocks)]] else nextBlock

      # Jump to the first condition block
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
      if(length(getTerminator(ir$getInsertBlock())) == 0)
         ir$createBr(nextBlock)  # jump to the end of the entire if statement           

      if(!is(cur, "if") || length(cur) < 4)
           break
      cur = cur[[4]]
      ctr = ctr + 1
    }
     
    ir$setInsertPoint(nextBlock)
}
