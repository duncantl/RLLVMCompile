
compileForLoop =
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
   }

  class(ans) = "ForLoop"
  createLoopCode(ans$var, ans$limits, ans$body, env, , ir = ir, nextBlock = nextBlock, label = deparse(call))
  ans
}



createLoopCode =
  #
  # This takes a variable, a length variable and bulds a loop.
  #
  #
function(var, limits, body, env, fun = env$.fun, ir = IRBuilder(module), module = NULL, nextBlock = NULL,
          label = ".")
{
browser()
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
  
   iv = ir$createLocalVariable(Int32Type, var)
   assign(var, iv, env)
   env$.types[[var]] = Int32Type
   len = ir$createLocalVariable(Int32Type, ".llen")
   assign(".llen", len, env)
   env$.types[[".llen"]] = Int32Type
   
   mapply(function(lim,  to) {
          if(is.symbol(lim)) {
            sym = as.character(lim)
            var = getVariable(sym, env, ir)
            # ir$createLoad(to, get(as.character(lim), env))
             ir$createStore(var, to)          
          } else
             ir$createStore(lim, to)
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
         }

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
