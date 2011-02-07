subsetHandler =
function(call, env, ir, ..., load = TRUE)
{
  obj = getVariable(call[[2]], env, ir)
  i = compile(call[[3]], env, ir) # getVariable(call[[3]], env, ir)  
  idx = ir$createSExt(i, 64L)
  p = ir$createGEP(obj, idx)
  if(load)
     ir$createLoad(p)
  else
     p
}


subsetAssignHandler =
function(call, env, ir, ...)
{
  browser()
  ll = subsetHandler(call, env, ir, load = FALSE)
  ir$createStore(tmp, ans.i)
}
  
