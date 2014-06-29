repeatHandler =
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL)
{
  e = quote(while(TRUE) {})
  e[[3]] = call[[2]]
  compile(e, env, ir, ..., fun = fun, name = name)
}
