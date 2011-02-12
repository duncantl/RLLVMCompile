mathHandler =
  #
  # This currently (Feb 7, 2pm) attempts to do a little bit
  # of figuring about the types of the two operands.
  # It just handles integer and numeric types for now.
  #
  #  This has to handle both binary and unary calls, e.g. - x
  #
  #
  #
function(call, env, ir, ..., isSubsetIndex = FALSE)  
{
<<<<<<< HEAD

   if(length(call) == 2)  {
         # unary operator - most likely -
      val = compile(call[[2]], env, ir, ...)
     return(createNeg(val, "xx", ir$getInsertBlock()))
     # return(ir$binOp(FMul, ir$createConstant(-1), val))
   }
      

  
     # Need to hadle the case where we have a literal and we can convert it to the
     # target type.
  
  if(any( lit <- sapply(call[-1], is.numeric))) { # literals
    targetType = getTypes(as.list(call[-1])[!lit][[1]], env)
  } else {
       #??? We may want to compile first and then determine types and then coerce.
       # Compute the type of each, returning the LLVM type objects, e.g. DoubleType
    types = lapply(call[-1], getTypes, env)
     # Collapse these two types to the "common" type
    targetType = getMathOpType(types)
  }
  isIntType = identical(targetType, Int32Type)
  e = lapply(call[-1], function(x)
                       if(is(x, "numeric")) {
                          if(isIntType)
                             createIntegerConstant(as.integer(x))
                          else
                             createDoubleConstant(as.numeric(x))
                       } else if(is.name(x)) {
                            # Potentially have to cast based on the target type
                         getVariable(x, env, ir)
                       } else
                         compile(x, env, ir, ...))

    # XXX Have to deal with different types.
  if(isIntType)
     codes = c("+" = Add, "-" = Sub, "*" = Mul, "-" = SDiv, "%/%" = SRem)
  else
     codes = c("+" = FAdd, "-" = FSub, "*" = FMul, "-" = FDiv, "%/%" = FRem)

  op = codes[ as.character(call[[1]]) ]

  ins = ir$binOp(op, e[[1]], e[[2]])
  ins
}
