compile.while = whileHandler =
  #
  # This generates the code corresponding to an while() {} loop in R.
  #
function(call, env, ir, ..., fun = env$.fun, nextBlock = NULL)
{
    label = deparse(call)

         # create blocks for evaluating the condition
         # the body and then where to jump to when we are finished
         # We'll make the nextBlock the location where subsequent
         # instructions are added to continue on from this while() command
    cond = Block(fun, sprintf("cond.%s", label))
    bodyBlock = Block(fun, sprintf("body.%s", label))
    
#    nextBlock = Block(fun, sprintf("after.%s", label))

#   pushNextBlock(env, nextBlock)
#   on.exit(popNextBlock(env))
   pushContinueBlock(env, cond)
   on.exit(popContinueBlock(env), add = TRUE)       

          # We have to explicitly jump from the current block
          # before the while() loop to our condition code.
   ir$createBr(cond)
    
    ir$setInsertPoint(cond)

    createConditionCode(call[[2]], env, ir, bodyBlock, nextBlock,
                           breakBlock = nextBlock, nextIterBlock = cond, ...)

     ir$setInsertPoint(bodyBlock)

     compile(call[[3]], env, ir, ..., breakBlock = nextBlock, nextIterBlock = cond, nextBlock = nextBlock)
    
#      if(!identical(ir$getInsertBlock(), incrBlock) && length(getTerminator(ir$getInsertBlock())) == 0) 
#       if(length(getTerminator(ir$getInsertBlock())) == 0)  
#           ir$createBr(cond)

   ir$setInsertPoint(nextBlock)    
}


isCompositeCond =
function(call)
{
  is.call(call) && as.character(call[[1]]) %in% c("&&", "||")
}

createConditionCode =
function(call, env, ir, bodyBlock, nextBlock, ...)
{
    compositeCond = isCompositeCond(call)
                           
    if(!compositeCond)  {
  
          # Same as in while() so consolidate
       a = compile(call, env, ir, nextBlock = nextBlock, ...)
          # We don't need to compare the value of a to 1 but can
          # expect that a is a logical value. 
       # ok = ir$createICmp(ICMP_SLT, a, ir$createIntegerConstant(1L))

       ir$createCondBr(a, bodyBlock, nextBlock)
    } else {

      if(isCompositeCond(call[[2]])) {

        alt.id = paste(deparse(call[[3]]), collapse = "")
        bl = Block(env$.fun, alt.id)
        blocks = list()
        if(as.character(call[[1]]) == "||") {
           blocks[[1]] = bodyBlock
           blocks[[2]] = bl
        } else {
            blocks[[1]] = bl
            blocks[[2]] = nextBlock
        }


          createConditionCode(call[[2]], env, ir, blocks[[1]], blocks[[2]])
          ir$setInsertPoint(bl)
          ans = createConditionCode(call[[3]], env, ir, bodyBlock, nextBlock)
          return(ans)
      }
      
         # Currently assumes only a || b and only two expressions, i.e. not a || b || c which is recursive.
         #
         #
         #
         #
       a = compile(call[[2]], env, ir)

       alt.id = paste(deparse(call[[3]]), collapse = "")
       bl = Block(env$.fun, alt.id)
       if(as.character(call[[1]]) == "||")
          ir$createCondBr(a, bodyBlock, bl)
       else
          ir$createCondBr(a, bl, nextBlock)

       ir$setInsertPoint(bl)
       if(isCompositeCond(call[[3]]))
            # we need to deal with the composite condition here
            # in the context of an if()/while() statement and blocks, not
            # just a regular stand-alone call to && or || which we might
            # handle differently
         createConditionCode(call[[3]], env, ir, bodyBlock, nextBlock)
       else {
          b = compile(call[[3]], env, ir)       
          ir$createCondBr(b, bodyBlock, nextBlock)
       }
    }
}
