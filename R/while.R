whileHandler =
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

          # We have to explicitly jump from the current block
          # before the while() loop to our condition code.
   ir$createBr(cond)
    
    ir$setInsertPoint(cond)
        # if call[[2]] were something like  length(x) or len
        # we'd have to make this into an actual comparison
       a = compile(call[[2]], env, ir)
          # We don't need to compare the value of a to 1 but can
          # expect that a is a logical value. 
       # ok = ir$createICmp(ICMP_SLT, a, ir$createIntegerConstant(1L))
       ir$createCondBr(a, bodyBlock, nextBlock)    

     ir$setInsertPoint(bodyBlock)
         # If there is a single expression in the body and no { we need
         # to call compile() rather than compileExpressions. We'll reduce
         # these two into one function soon.
       compileExpressions(call[[3]], env, ir)
       ir$createBr(cond)

    ir$setInsertPoint(nextBlock)    
}
