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
  if(length(call) == 2)  {
    # unary operator - most likely -
    val = compile(call[[2]], env, ir, ...)
        
    ## TODO fix ir$ SS, clean this
    if (identical(getType(call[[2]], env), Int32Type)) {
      return(createNeg(val))
       # return(createNeg(ir, val, call[[2]]))  #?? as.character(call[[2]])))     
    }
    
    if (identical(getType(call[[2]], env), DoubleType)) {
      return(createFNeg(ir, val, as.character(call[[2]])))
    }
    stop("cannot createNeg for this type yet.")
   }
      

  
  # Need to handle the case where we have a literal and we can convert it to the
  # target type.
  toCast = NULL
   
  types = lapply(call[-1], getTypes, env)
  if(any(sapply(types, is.null)))
     stop("NULL value for component type in math operation")
    

  lit <- sapply(call[-1], is.numeric) # literals

  if(any(lit)) {
    # This has the problem that the literal will be coerced to the
    # other type, a non-R behavior. TODO remove entirely?
    ## targetType = getTypes(as.list(call[-1])[!lit][[1]], env)
  }

  # Collapse these two types to the "common" type
  targetType = getMathOpType(types)

  # If any of the types are different from the targetType, we need
  # to cast.
  typeMatches = sapply(types, identical, targetType)
  if (any(!typeMatches))
    toCast = as.list(call[-1])[[which(!typeMatches)]]

  isIntType = identical(targetType, Int32Type)
  e = lapply(call[-1], function(x)
                       if(is(x, "numeric")) {
                          if(isIntType)
                            createIntegerConstant(as.integer(x))
                          else
                            createDoubleConstant(as.numeric(x))
                        } else if(is.name(x)) {
                          if (!is.null(toCast) && x == toCast) {
                            # Casting to double needed
                            return(createCast(ir, DoubleType, Int32Type,
                                             getVariable(x, env, ir)))
                         } else 
                           getVariable(x, env, ir)
                       } else
                         compile(x, env, ir, ...))

    # XXX Have to deal with different types.
  if(isIntType)
     codes = c("+" = Add, "-" = Sub, "*" = Mul, "-" = SDiv, "%/%" = SRem)
  else
     codes = c("+" = FAdd, "-" = FSub, "*" = FMul, "-" = FDiv, "%/%" = FRem)

  op = codes[ as.character(call[[1]]) ]

  ins = ir$binOp(op, e[[1]], e[[2]], id = deparse(call))
  ins
}
