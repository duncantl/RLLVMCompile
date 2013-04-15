rewriteExpressions =
function(expr, env)
{
  if(is.name(expr) && as.character(expr) %in% names(env$.Constants))
     env$.Constants[[ as.character(expr)]]
  else
     expr
}
