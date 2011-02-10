compile.while = whileHandler =
  #
  # This generates the code corresponding to an while() {} loop in R.
  #
function(call, env, ir, ..., fun = env$.fun)
{
    label = deparse(call)

         # create blocks for evaluating the condition
         # the body and then where to jump to when we are finished
         # We'll make the nextBlock the location where subsequent
         # instructions are added to continue on from this while() command
    cond = Block(fun, sprintf("cond.%s", label))
    bodyBlock = Block(fun, sprintf("body.%s", label))
    nextBlock = Block(fun, sprintf("next.%s", label))

   pushNextBlock(env, nextBlock)
   on.exit(popNextBlock(env))
   pushContinueBlock(env, cond)
   on.exit(popContinueBlock(env))       

          # We have to explicitly jump from the current block
          # before the while() loop to our condition code.
   ir$createBr(cond)
    
    ir$setInsertPoint(cond)
    createConditionCode(call[[2]], env, ir, bodyBlock, nextBlock)

     ir$setInsertPoint(bodyBlock)
       compile(call[[3]], env, ir)
       ir$createBr(cond)

    ir$setInsertPoint(nextBlock)    
}



createConditionCode =
function(call, env, ir, bodyBlock, nextBlock)
{
          # Same as in while() so consolidate
       a = compile(call, env, ir)
          # We don't need to compare the value of a to 1 but can
          # expect that a is a logical value. 
       # ok = ir$createICmp(ICMP_SLT, a, ir$createIntegerConstant(1L))

       ir$createCondBr(a, bodyBlock, nextBlock)    
}
