
compile.dollar = `compile.$` =
function(call, env, ir, ..., .targetType = NULL)
{  
       elName = as.character(call[[3]])
       obj = call[[2]]
       val = compile(obj, env, ir)
       ty = valType = getType(val)
       pointerToStruct = isPointerType(ty)
       if(pointerToStruct)
         valType = getElementType(ty)


       if(isStructType(valType)) {

           # Should add setAlignment() calls (with 8L) for the local var, store and load instructions
                # make local variable to access the parameter. TEMPORARY. See if it is already present.
#XXX         
          pvar = createLocalVariable(ir, ty, sprintf("%s.addr", as.character(call[[2]]))) # not if call[[2]] is an actual call

          ans = createStore(ir, val, pvar) # ??? should val be compiled or just get the parameter value.

          tmp = createLoad(ir, pvar)

          
            # now match the name of the field being accessed to its position in the structure.
            # we need the structure info from outside of llvm as it doesn't store names.
          info = findStructInfo(valType, env$.structInfo)
          if(is.null(info))
            stop("need information about the elements of the struct")
          
          index = match(elName, info@names) - 1L
          if(is.na(index))
            stop("no such field '", elName,  "' in the struct")

          ctx = getContext(env$.module)
          elVal = createGEP(ir, tmp, lapply(c(0L, index), createIntegerConstant, ctx), "getfield")
          
          ans = createLoad(ir, elVal)
          setAlignment(ans, 4L)          
       }
       else
         stop("not implemented yet")
       
   ans
}

findStructInfo =
function(ty, info)
{

  i = sapply(info, sameType, ty)
  if(!any(i))
    return(NULL)

  info[[which(i)[1]]]
}

