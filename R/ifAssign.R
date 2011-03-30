#
# This takes care of rewriting expressions of the form
#   expr = if(....)
# which R supports, but native code won't.
#
# See inst/examples/ifAssign.R

fixIfAssign =
function(expr, var = character(), ...)
  UseMethod("fixIfAssign")

fixIfAssign.default =
function(expr, var = character(), ...)
  expr

fixIfAssign.function =
function(expr, var = character(), ...)
{
    #XXX Default values also
   body(expr) = fixIfAssign(body(expr), recurse = TRUE)
   expr
}

`fixIfAssign.{` =
function(expr, var = character(), recurse = FALSE, ...)
{
   if(recurse) {
     expr[-1] = lapply(expr[-1], fixIfAssign)
     return(expr)
   } else if(length(var)) {
      n = length(expr)
      expr[[n]] = fixIfAssign(expr[[n]], var)
   }

   expr
}

`fixIfAssign.=` = `fixIfAssign.<-`  =
  #
  # If the variable to which we are assigning the result of the if
  # does not already exist, then we have problems when we generate the code.
  #
  #
function(expr, var = character(), ...)
{
   if(is(expr[[3]], "if")) {
      expr[[3]][[3]] = fixIfAssign(expr[[3]][[3]], expr[[2]], recurse = FALSE, ...)
      if(length(expr[[3]]) >= 4)
         expr[[3]][[4]] = fixIfAssign(expr[[3]][[4]], expr[[2]], recurse = FALSE, ...)

      expr[[3]]
   } else
     expr
}  


`fixIfAssign.call` =
function(expr, var = character(), ...)    
{
  if(length(var))
    assignTo(var, expr)
  else
     expr
}


assignTo =
function(var, expr)
{
   e = quote(x <- val)
   e[[2]] = var
   e[[3]] = expr
   e
}
