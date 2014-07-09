compile.if = ifHandler =
  #
  # Generate code for an if statement
  #
function(call, env, ir, ..., fun = env$.fun, continue = FALSE, nextBlock = NULL)
{
      # check for simple degenerate cases of if(FALSE) or if(TRUE) and don't compile the relevant pieces.
    e = call
    if(e[[2]] == TRUE) {
        val = compile(e[[3]], env, ir, ..., nextBlock = nextBlock) 
        return( val  )
    } else if(e[[2]] == FALSE) {
        if(length(e) == 3)
            return(NULL)
        else
        return( compile(e[[4]], env, ir, ..., nextBlock = nextBlock) )
    }


    
   # This is not elegant, but brute force.
   # We basically create blocks for each of the if conditions
   # and blocks for each of the bodies, including a trailing else-clause
   # block, if it is present.  The function also  adds a new block at the end which is where
   # we will end up at the conclusion of the if.... regardless of how we get through it.
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
      # This loop just creates the Blocks, not the code within those blocks
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

         if(length(cur) == 4)  # so there is an else
            cur = cur[[4]]
         else
           break
     }

      # This is the block that all branches of the if statement
      # will end up in. This is where start the code for the next statement
      # after the if statement.
#??? How do we know the label
#if(!is.null(nextBlock)) { cat("passed a nextBlock\n"); browser()}

if(is.null(nextBlock)) {
#  cat("[compile.if] creating our own nextBlock\n");   browser()
   if(length(env$.remainingExpressions))
       nextBlock = Block(fun, sprintf("after_if.%s", label))
   else {
#       nextBlock = NULL
#nextBlock = env$.nextBlock[[1]]
nextBlock = getNextBlock(env)
   }
}

    condBlocks[["final"]] = if(hasTrailingElse) bodyBlocks[[length(bodyBlocks)]] else nextBlock

      # Jump to the first condition block. We could probably execute the condition test code in the current block. But ....
    ir$createBr(condBlocks[[1]])

    #??? Does this code end up jumping to the trailing else ?
    cur = call
    ctr = 1
    while(TRUE) {
      body = bodyBlocks[[ctr]]
           # alt is a bad name. Should be conditionBlock or curConditionBlock
           # condBlocks[[ctr]] and nextBlock may be identical in some cases (see simplerNestedIfLoop.R)
      alt = if(ctr <= length(condBlocks))
               condBlocks[[ctr]]
            else
               nextBlock
      
      if(is(cur, "if")) {
           ir$setInsertPoint(alt)

           if(ctr >= length(condBlocks)) {
               if(length(env$.remainingExpressions)) 
                   stop("something has gone wrong here. Please report this as a bug in RLLVMCompile")

#browser()               
#   if(length(env$.nextBlock) == 0), we can create a new block and add it to env$.nextBlock, and also
# condBlocks and then go ahead and hope for good things to happen, but probably an error in the user's code.
#              if(sameType(env$.returnType, VoidType)) {
#                  condBlocks[[ctr + 1]] = env$.nextBlock = Block(env$.fun)
#                 .cur = getInsertBlock(ir)
#                 env$.remainingExpressions = list(quote(return()))
#                 setInsertBlock(ir, env$.nextBlock)
#                 createRetVoid(ir)
#                 setInsertBlock(ir, .cur)
#              } else
                  stop("no condition block to jump to. Is there an expression following this if expression?", class = c("UserCodeError", "CompileError"))
           }


           createConditionCode(cur[[2]], env, ir, body, condBlocks[[ctr + 1]], ...)
           ir$setInsertPoint(body)
           compile(cur[[3]], env, ir, ..., nextBlock = nextBlock)           
       } else {
           ir$setInsertPoint(body)
           compile(cur, env, ir, ..., nextBlock = nextBlock)                      
       }
      
      if(length(getTerminator(ir$getInsertBlock())) == 0) {
        if(!is.null(nextBlock)) {
#print(getName(getInsertBlock(ir)))
#print(getName(nextBlock))
#browser()
          if(! identical(nextBlock, getInsertBlock(ir)) )
             ir$createBr(nextBlock)  # jump to the end of the entire if statement
      } else if(!is.null(condBlocks[["final"]]) && length(getTerminator(condBlocks[["final"]])) == 0) {
#XXX TIDY THIS MESS UP after all the explorations relating to the compile.if and 2DRandomWalk.Rdb.
       #  ir$createBr(condBlocks[["final"]])
      } else if(length(env$.nextBlock)) {  # THIS IS PROBABLY THE WRONG THING TO DO AND SHOULD GO. IT WAS JUST A NEVER WORKING EXPERIMENT.
          ir$createBr(env$.nextBlock[[1]])
      } else
        stop("need a terminator")
    }

      if(!is(cur, "if") || length(cur) < 4)
           break
      cur = cur[[4]]
      ctr = ctr + 1
  }

    if(!is.null(nextBlock))
       ir$setInsertPoint(nextBlock)
}


getNextBlock =
function(env)
{
    #TEMPORARY
#  if(length(env$.nextBlock))
#      return(env$.nextBlock[[1]])
  
  if(length(env$.continueBlock))
      return(env$.continueBlock[[1]])

  if(length(env$.nextBlock))
      return(env$.nextBlock[[1]])  

  NULL
}    
