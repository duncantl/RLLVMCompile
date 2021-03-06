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
# if(FALSE && length(call) == 2)  {
#    # unary operator - most likely -
#    val = compile(call[[2]], env, ir, ...)
#
#    ty2 = getTypes(call[[2]], env)
#    ## TODO fix ir$ SS, clean this
#    if (identical(ty2, Int32Type)) {
#      return(createNeg(val))
#       # return(createNeg(ir, val, call[[2]]))  #?? as.character(call[[2]])))     
#    }
#    
#    if (identical(ty2, DoubleType)) {
#      return(createFNeg(ir, val, as.character(call[[2]])))
#    }
#
#    stop("cannot createNeg for this type yet.")
# }


  if(length(call) == 2) {  # So a unary operation
       #XXX temporary exploration
        # if this is +, e.g. +n, we should just compile call[[2]]
     if(as.character(call[[1]]) == "+")
        return(compile(call[[2]], env, ir, ..., isSubsetIndex = isSubsetIndex))

      # XXX what about !
     k = quote(0 - 0)
     k[[3]] = call[[2]]
     k[[1]] = call[[1]]
     call = k
 }  # Finished with unary operation
 
  call[2:length(call)] = lapply(call[-1], rewriteExpressions, env, isSubsetIndex = isSubsetIndex)

  origCall = call

  lit <- sapply(call[-1], is.numeric) # literals

  if(all(lit)) {
          # The two operands are literal values so no need to compile run-time code.
          # So compute the result here and return.
    value = eval(call)
    if(is.numeric(value) && env$.useFloat)
       return(createFloatingPointConstant(value, getContext(env$.module), FloatType))
    else
       return(createConstant(, value, context = getContext(env$.module)))
  }    

  
   # Need to handle the case where we have a literal and we can convert it to the
   # target type.
  toCast = NULL

     # we are getting the types here w/o compiling the expressions (?). So they may not be what we end up with.
  types = lapply(call[-1], getTypes, env)
  if(any(nulls <- sapply(types, is.null))) {
     i = which(nulls) + 1L
     call[i] = lapply(call[i], compile, env, ir, ...)
     types[nulls] = lapply(call[-1][nulls], getType)
#     stop("NULL value for component type in math operation")
  }


  if(any(lit)) {
    # This has the problem that the literal will be coerced to the
    # other type, a non-R behavior. TODO remove entirely?
    ## targetType = getTypes(as.list(call[-1])[!lit][[1]], env)
    
    targetType = getMathOpType(types[!lit])
    toCast = lit
  } else  {

           # Collapse these two types to the "common" type
    targetType = getMathOpType(types)

          # If any of the types are different from the targetType, we need
          # to cast.
    typeMatches = sapply(types, sameType, targetType)
    if(any(!typeMatches))
       toCast = as.list(call[-1])[[which(!typeMatches)]]
  }

  isIntType = sameType(targetType, Int32Type) || sameType(targetType, Int64Type)
  e = lapply(call[-1], function(x) {

                       if(is(x, "Value")) {
                           if(is.null(toCast))
                              x
                           else
                             createCast(env, ir, targetType, Rllvm::getType(x), x)
                       } else if(is(x, "numeric")) {

                          if(isIntType)
                            createIntegerConstant(as.integer(x))
                          else if(sameType(targetType, DoubleType))
                            createDoubleConstant(as.numeric(x))
                          else
                            createFloatingPointConstant(as.numeric(x), type = FloatType)
                       } else if(is.name(x)) {
                          if (!is.null(toCast) && identical(x, toCast)) {
                                # Casting to double needed
                            var = getVariable(x, env, ir)
                            return(createCast(env, ir, targetType, Rllvm::getType(var), var))
                         } else 
                           getVariable(x, env, ir)
                       } else
                         compile(x, env, ir, ...)
                     })

    # XXX Have to deal with different types.
  if(isIntType)
     codes = c("+" = "Add", "-" = "Sub", "*" = "Mul", "/" = "SDiv", "%/%" = "SRem")
  else 
     codes = c("+" = "FAdd", "-" = "FSub", "*" = "FMul", "/" = "FDiv", "%/%" = "FRem")


  opName = as.character(call[[1]])
  if(opName %in% names(codes))
    op = structure(BinaryOps[[ codes[ opName ] ]], names = opName)
  else
    op = structure(NA, names = opName)

  if(any(w <- sapply(e, is, "Constant"))) {
      # if any of the operands are constants
     if(all(w)) { # constant fold
         warning("should constant fold here")
     } else {
       idx = which(w)
       k = e[[ idx ]]
       v = getValue(k)
       if( (v == 0 && names(op) == "+") ||
           (v == 0 && idx == 2L && names(op) == -1) ||
           (v == 1 && names(op) == "*") ||
            (v == 1 && names(op) == "/" && which(w) == 2))
                      return(e[!w][[1]])
    }
  }

  if(!is.na(op)) {
      ins = ir$binOp(op, e[[1]], e[[2]], id = deparse(origCall))
  } else {
    
     if(opName == "^") {
         # also see callHandler() in call.R
       f = declareFunction(getBuiltInRoutines(env = env)[["pow"]], "pow", env$.module)
       env$addCallInfo("pow")       
       ins = ir$createCall(f, e[[1]], e[[2]])
     } else
       stop("no math operation found corresponding to ", opName)
  }

  ins
}
