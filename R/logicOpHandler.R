logicOpHandler =
# This handles all logical operators, binary and unary (which is only
# !). This includes: ==, !=, >, <, >=, <= for both real and integer
# values (so far). Type coercion is needed, as LLVM has type-specific
# comparison operators like FCmp.
function(call, env, ir, ...)
{
#if(call[[1]] == as.name("!"))  browser()
   if(length(call) == 2) {
     val = compile(call[[2]], env, ir, ...)
     ty.val = getType(val)
     if(sameType(ty.val, Int1Type))
       not = ir$createNot(val)
     else if(isIntegerType(ty.val))  { #sameType(getType(val), Int32Type)) {
        # could just rewrite the call to call[[2]] == 0 or whatever and then pass to code below
        # but we have already compiled call[[2]].
       not = ir$createICmp( ICMP_EQ, val, ir$createIntegerConstant(0L, getContext(env$.module)), paste(deparse(call[[2]]), collapse = ""))
     } else
        stop("Not yet handling ! for non-integer types")
     
     return(not) #, "xx", ir$getInsertBlock()))
   }
  
    # need to handle the different ops
    # and the different types, casting
    # if necessary.
  op = as.character(call[[1]])

  types = lapply(call[-1], getTypes, env)
  targetType = getMathOpType(types)
  isIntType = identical(targetType, Int32Type)  

  a = compile(call[[2]], env, ir)   # getVariable(call[[2]], env, ir)
  b = compile(call[[3]], env, ir)   # getVariable(call[[3]], env, ir)

  if(isIntType)
     codes = c("==" = ICMP_EQ, "!=" = ICMP_NE, ">" = ICMP_SGT, "<" = ICMP_SLT, ">=" = ICMP_SGE, "<=" = ICMP_SLE)
  else {
    codes = c("==" = FCMP_UEQ, "!=" = FCMP_UNE, ">" = FCMP_UGT, "<" = FCMP_ULT, ">=" = FCMP_UGE, "<=" = FCMP_ULE)

    # Coerce type TODO replace with more generic type coercion?
#    browser()
    vars = list(a, b)
    convert.i = which(sapply(types, function(x) !identical(x, targetType)))
    vars[[convert.i]] = createCast(ir, targetType, types[[convert.i]], vars[[convert.i]])
    a = vars[[1]]
    b = vars[[2]]
  }
  op = codes[ as.character(call[[1]]) ]
  if(is.na(op))
     stop("Unhandled logical operator")

  if(isIntType)
    ir$createICmp(op, a, b)    
  else
    ir$createFCmp(op, a, b)
}
