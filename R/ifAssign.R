#
# This takes care of rewriting expressions of the form
#   expr = if(....)
# which R supports, but native code won't.
#
# See inst/examples/ifAssign.R
#
#????  Should we add something of the form pragma("IfAssign",  varName, expr)
# and then have the compiler recognize & use this.

fixIfAssign =
function(expr, var = character(), ...)
  UseMethod("fixIfAssign")

fixIfAssign.default = fixIfAssign.name =
function(expr, var = character(), ...)
{
  if(length(var))
     assignTo(var, expr)
  else
     expr
}

fixIfAssign.numeric = fixIfAssign.character = fixIfAssign.logical =
function(expr, var = character(), ...)
{
    if(length(var))
       substitute(v <- val, list(v = var, val = expr))
    else
      expr
}

fixIfAssign.while =
function(expr, var = character(), ...)
{
   expr[[3]] = fixIfAssign(expr[[3]], var, ...)
   expr
}

fixIfAssign.function =
function(expr, var = character(), ...)
{
    #XXX Default values also
   body(expr) = fixIfAssign(body(expr), recurse = TRUE)
   expr
}

`fixIfAssign.next` = `fixIfAssign.continue` =
function(expr, var = character(), recurse = FALSE, ...)
    expr


`fixIfAssign.if` =
function(expr, var = character(), recurse = FALSE, ...)
{
    expr[[3]] = fixIfAssign(expr[[3]], var, ..., recurse = recurse)
    if(length(expr) >= 4)
       expr[[4]] = fixIfAssign(expr[[4]], var, ..., recurse = recurse)
    expr
}


`fixIfAssign.{` =
function(expr, var = character(), recurse = TRUE, ...)
{

   if(recurse) {
      expr[-1] = lapply(expr[-1], fixIfAssign, recurse = recurse)
     return(expr)
   } else if(length(var)) {
      n = length(expr)
      expr[[n]] = fixIfAssign(expr[[n]], var, recurse = recurse)
   }

   expr
}

`fixIfAssign.for` =
function(expr, var = character(), recurse = FALSE, ...)
{
   expr[[4]] = fixIfAssign(expr[[4]], var, recurse = TRUE)

   expr
}



`fixIfAssign.=` =  `fixIfAssign.<-`  =
  #
  # If the variable to which we are assigning the result of the if
  # does not already exist, then we have problems when we generate the code.
  #
  #
function(expr, var = character(), addPragma = FALSE, ...)
{
   if(is(expr[[3]], "if")) {
      expr[[3]][[3]] = fixIfAssign(expr[[3]][[3]], expr[[2]], recurse = TRUE, ...)
      if(length(expr[[3]]) >= 4)
         expr[[3]][[4]] = fixIfAssign(expr[[3]][[4]], expr[[2]], recurse = TRUE, ...)

      if(addPragma)
          substitute(pragma(IfAssign, v, e), list(v = expr[[2]], e = expr[[3]]))
      else
         expr[[3]]  # remove the a = if() and just give back the updated if
         
      
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
