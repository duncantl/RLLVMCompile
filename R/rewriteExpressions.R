rewriteExpressions =
function(expr, env, isSubsetIndex = FALSE)
{

  if(is.name(expr) && as.character(expr) %in% names(env$.Constants))
     env$.Constants[[ as.character(expr)]]
  else if(isSubsetIndex && is.numeric(expr))
     as.integer(expr)
  else if(is.call(expr)) {
     for(i in seq(2, length = length(expr) - 1))
        expr[[i]] =  rewriteExpressions(expr[[i]], env, isSubsetIndex)
     expr
  } else
     expr
}
