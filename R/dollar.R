
compile.dollar = `compile.$` =
function(call, env, ir, ...)
{  
       elName = as.character(call[[3]])
       obj = call[[2]]
       val = compile(obj, env, ir)
       ty = valType = getType(val)
       pointerToStruct = isPointerType(ty)
       if(pointerToStruct)
         valType = getElementType(ty)

#browser()       
       if(isStructType(valType)) {
                # make local variable to access the parameter. TEMPORARY. See if it is already present.
          pvar = createLocalVariable(ir, ty, sprintf("%s.addr", as.character(call[[2]]))) # not if call[[2]] is an actual call
#          setAlignment(pvar, 8L)
          ans = createStore(ir, val, pvar) # ??? should val be compiled or just get the parameter value.
#          setAlignment(ans, 8L)
          tmp = createLoad(ir, pvar)
#          setAlignment(tmp, 8L)
          
          ctx = getContext(env$.module)
                 # need to change indices based on the actual name of the struct.
          elVal = createGEP(ir, tmp, lapply(c(0L, 0L), createIntegerConstant, ctx), "getfield")
          
          ans = createLoad(ir, elVal)
          setAlignment(ans, 4L)          
          return(ans)
       }
       else
         stop("not implemented yet")
       
   ans
}
