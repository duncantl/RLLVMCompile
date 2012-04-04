subsetHandler =
# Handle subsetting a vector of any type.
#
# References:
#  - GEP: http://llvm.org/docs/LangRef.html#i_getelementptr
#  - SExt: http://llvm.org/docs/LangRef.html#i_sext
function(call, env, ir, ..., load = TRUE) {
  obj = getVariable(call[[2]], env, ir)
  i = compile(call[[3]], env, ir) # getVariable(call[[3]], env, ir)
  #i = getVariable(call[[3]], env, ir)
  idx = ir$createSExt(i, 64L)
  p = ir$createGEP(obj, idx)
  if(load)
    return(ir$createLoad(p))
  return(p)
}


subsetAssignHandler =
  #
  # Never used!
  #
function(call, env, ir, ...)
{
  ll = subsetHandler(call, env, ir, load = FALSE)
#  ir$createStore(tmp, ans.i)
}
  
