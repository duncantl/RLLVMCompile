logicOpHandler =
function(call, env, ir, ...)
{
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
  else
     codes = c("==" = FCMP_UEQ, "!=" = FCMP_UNE, ">" = FCMP_UGT, "<" = FCMP_ULT, ">=" = FCMP_UGE, "<=" = FCMP_ULE)    
  
  op = codes[ as.character(call[[1]]) ]
  if(is.na(op))
     stop("Unhandled logical operator")

  if(isIntType)
    ir$createICmp(op, a, b)    
  else
    ir$createFCmp(op, a, b)
}
