
compile.for = compileForLoop =
function(call, env, ir, ..., nextBlock = NULL)
{
  var = as.character(call[[2]])
  inn = call[[3]]
  isSeq = isSequence(inn)
  ans = list(var = var, body = call[[4]])
  if(isSeq) {
     ans$limits = getLimits(inn)
  } else {
       # we have a call to something and so
       # may need to create a temporary variable
       # Then we need to loop over those
       # and so create limits from that object, i.e.
       # 1 to length(var).
       # if the inn is a symbol, then we loop over its elements.
       # We need to know its type.
     if(!is.name(inn)) {
         # create temporary variable by evaluating the call
         # and then create the limits for that.
         # compile the call and assign to a new variable, then
         # loop over that.
         tmpVar = "tmp"
         ans$tempVar = structure(inn, name = tmpVar)
         ans$limits = list(from = 1L, to = substitute(length(x), list(x = as.name(tmpVar))))
     } else
         ans$limits = list(from = 1L, to = substitute(length(x), list(x = call[[3]])))

     #XXX since isSeq is FALSE here, we have to introduce a new temporary variable
     # for the counter variable and rewrite the body of the loop or assign the value
     # to the temporary variable. e.g. for(val in x) ... would become for(i in seq(along = x)) { val = x[i]; body }
     # or else we have to change references to val to x[val].  For debugging, rewriting can be aggrevating.
     # Introducing a new variable could conflict with another in the function, so need to know all of those.
     if(class(ans$body) != "{") 
       ans$body = substitute({foo}, list(foo = ans$body))

     b = ans$body
     b[(2:length(body)) + 1L] = b[(2:length(body))]
     e = quote(x <- y)
     e[[2]] = as.name(ans$var)
     ans$var = "i"  #XXX pick an unused variable name
     warning("compute name for counter in loop to be unique")
     e[[3]] = substitute(y[i], list(y = inn))
     b[[2]] = e
     ans$body = b
   }

  class(ans) = "ForLoop"
  createLoopCode(ans$var, ans$limits, ans$body, env, , ir = ir, nextBlock = nextBlock, label = deparse(call))
  ans
}



createLoopCode =
  #
  # This takes a variable, a length variable and builds a loop.
  #
  #
function(var, limits, body, env, fun = env$.fun, ir = IRBuilder(module), module = NULL, nextBlock = NULL,
          label = ".", zeroBased = FALSE)
{
   env$.loopDepth = env$.loopDepth + 1L
   on.exit( env$.loopDepth <- env$.loopDepth - 1L)
  
     # The caller (compileFunction and compileExpressions) has already created a block
     # for this expression, so we can use it as the entry block and create and initialize
     # variables here.

      # We do create blocks for the condition and the body.
   cond = Block(fun, sprintf("cond.%s", label))
   incrBlock = Block(fun, sprintf("incr.%s", label))   
   bodyBlock = Block(fun, sprintf("body.%s", label))
   nextBlock = Block(fun, sprintf("next.%s", label))

   pushNextBlock(env, nextBlock)
   on.exit(popNextBlock(env))
   pushContinueBlock(env, incrBlock)
   on.exit(popContinueBlock(env))   
  
   iv = createFunctionVariable(Int32Type, var, env, ir)  #  ir$createLocalVariable(Int32Type, var)
   assign(var, iv, env)
   env$.types[[var]] = Int32Type
   len = createFunctionVariable(Int32Type, ".llen", env, ir) # ir$createLocalVariable(Int32Type, ".llen")
   assign(".llen", len, env)
   env$.types[[".llen"]] = Int32Type
   mapply(function(lim,  to) {
     if(is.symbol(lim)) {
       sym = as.character(lim)
       var = getVariable(sym, env, ir)
       # ir$createLoad(to, get(as.character(lim), env))
       ir$createStore(var, to)
     } else if(is.call(lim) && length(lim) == 2 && as.character(lim[[1]]) == "length") {
        ty = getTypes(lim[[2]], env)
        if(is(ty, "SEXPType")) {
             # declare Rf_length()
           R.length = declareFunction(getBuiltInRoutines(env)[["length"]], "Rf_length", env$.module)
           sym = as.character(lim[[2]])
           var = getVariable(sym, env, ir)
#XXXXXX
           env$addCallInfo("Rf_length")           
           ir$createStore(ir$createCall(R.length, var), to)
        } else
           stop("Not certain  what to do with ", paste(deparse(lim), collaspe = " "), " for loop extents")
     } else {
       ir$createStore(lim, to)
     }

     if(zeroBased) {
                  # offset the loop vars by one, since LLVM is 0-indexed and R is
                  # 1-indexed. This is a bit clunky, but LLVM will optimize this
                  # all away.
       offset = ir$createLoad(to)
       offset.var = ir$binOp(Sub, offset, 1L)
       ir$createStore(offset.var, to)
     }
   }, limits, list(iv, len))
   
   ir$createBr(cond)

   ir$setInsertPoint(cond)   
   a = ir$createLoad(iv)
   b = ir$createLoad(len)

   ok = ir$createICmp(ICMP_SLE, a, b)
   ir$createCondBr(ok, bodyBlock, nextBlock)
   
   ir$setInsertPoint(bodyBlock)

           #XXX have to put the code for the actual  body, not just the incrementing of i

     compile(body, env, ir)
     ir$createBr(incrBlock)

   ir$setInsertPoint(incrBlock)

     i = ir$createLoad(iv)
     inc = ir$binOp(Add, i, 1L)
     ir$createStore(inc, iv)
     ir$createBr(cond)

   ir$setInsertPoint(nextBlock)
}

## offsetIndex <- 
## # Offset indexing by one to account for LLVM 0-based indexing and R's
## # 1-based indexing
## function(x) {
##   browser()
##   # TODO is this safe? could there be a name clash with y?
##   x$from <- substitute(expression(y - 1), list(y=x$from))
##   x$to <- substitute(expression(y - 1), list(y=x$to))
##   x
## }

getLimits =
  #  1:10
  #  1:length(x)
  #  seq(1, 10)
  #  seq(1, 10, by = 3)
  #  seq(1, 10, length = 2)  
  #  seq.int(0, 10, 2)
  #  seq.int(0, 10, length = 3)  
  # 
  #  seq_along(x)
  # getLimits(quote(seq(0, 2^n, length = 2)))
  # getLimits(quote(seq(0, length(x), length = 2)))
  #
function(call)
{
  op = as.character(call[[1]])
  
  ans = if(op == ":") {
    # TODO fix compiling of these calls
    ## list(from = compile(call[[2]], env, ir, ...),
    ##      to = compile(call[[3]], env, ir, ...))
    list(from = call[[2]], to = call[[3]])
  } else if(op == "seq_along") {
    tmp = substitute(length(x), list(x = call[[2]]))
    list(from = 1L, to = tmp)
  } else if(op == "seq") {
    k = match.call(seq, call)
    argNames = names(k)[-1]
    # formals(seq) returns ...
    formals = c("from", "to", "by", "length.out", "along.with")
    i = argNames == ""
    argNames[i] = formals[which(i)]
    structure(as.list(call[-1]), names = argNames)
    warning("TODO this may not work with R to LLVM indexing offsetting.")
  }

  ## # LLVM uses 0 indexing and R doesn't; this we offset by 1
  ## ans <- offsetIndex(ans)
  
  # how should we get integers when we have, e.g., 1:10 which are
  # numeric
  ans = lapply(ans, function(val) if(is.numeric(val) && val == as.integer(val)) as.integer(val) else val)
  
  ans
}



isSequence =
function(expr)
{
  if(is.call(expr)) {
    op = expr[[1]]
    if(as.character(op) %in% c("seq", ":", "seq_len", "seq.int", "seq_along"))
      return(TRUE)
  }

  FALSE
}


pushNextBlock =
function(env, block)
{
  env$.nextBlock = c(block, env$.nextBlock)
}

popNextBlock =
function(env)
{
  env$.nextBlock = env$.nextBlock[-1]
}

pushContinueBlock =
function(env, block)
{
  env$.continueBlock = c(block, env$.continueBlock)
}

popContinueBlock =
function(env)
{
  env$.continueBlock = env$.continueBlock[-1]
}
