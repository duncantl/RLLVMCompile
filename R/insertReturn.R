insertReturn =
  # Checks to see if we need to enclose the final expression
  # within a call to return()
  #
  # insertReturn(quote(return(x + 1))  )
  # insertReturn(quote(x + 1))
  # insertReturn(quote({ x = 2; x + 1} ))
  # insertReturn(quote({ x = 2; return(x + 1)} ))
  # insertReturn(quote(while(TRUE) {  return(x + 1) }  ))
  # insertReturn(quote(while(TRUE) {  x + 1 }  ))
  # insertReturn(quote(if(x < 10) 20 else 40  ))
  # insertReturn(quote(if(x < 10) { x= 3; sqrt(x) } else 40  ))
  # insertReturn(quote(if(x < 10) { x= 3; sqrt(x) } else { x = 100; sqrt(x)}  ))      
  #
  #XXX Need to handle while, if  
function(expr, nested = FALSE, isVoid = FALSE, ...)
  UseMethod("insertReturn")


`insertReturn.{` =
function(expr, nested = FALSE, isVoid = FALSE, ...)
{
     expr[[length(expr)]] = insertReturn(expr[[length(expr)]], nested)
     expr
}

insertReturn.name =
function(expr, nested = FALSE, isVoid = FALSE, ...)
{
   substitute(return(x), list(x = expr))
}


`insertReturn.call` =
function(expr, nested = FALSE, isVoid = FALSE, env = NULL, ...)
{
#XXXX rework this strategy
  if(!is.null(env) && !is.null(getSApplyType(expr, env)))
    return(expr)
  
  if(nested || expr[[1]] != as.name('return')) {
     substitute(return(x), list(x = expr))
  } else
    expr
}

insertReturn.if =
function(expr, nested = FALSE, isVoid = FALSE, ...)
{
  expr[[3]] = insertReturn(expr[[3]], nested = TRUE)
  if(length(expr) == 4)
      expr[[4]] = insertReturn(expr[[4]], nested = TRUE)
  expr
}

insertReturn.numeric = insertReturn.logical = insertReturn.character =
  insertReturn.integer = `insertReturn.(` =     # should check for (return(x+1))
   function(expr, nested = FALSE, isVoid = FALSE, ...) {
     k = call('return')
     k[[2]] = expr
     k
   }


insertReturn.while =
function(expr, nested = FALSE, isVoid = FALSE, ...)
{
  expr[[3]] = insertReturn(expr[[3]], nested = TRUE)
  expr
}

`insertReturn.=` = `insertReturn.<-` =
function(expr, nested = FALSE, isVoid = FALSE, ..., value = NULL)
{
  substitute(return(x), list(x = expr))
}


`insertReturn.function` =
function(expr, nested = FALSE, isVoid = FALSE, ...)
{
#   body(expr) = insertReturn(body(expr))
   b = body(expr)
   if(class(b) == "{") {
      b[[length(b)]] = if(FALSE && isSelect(b[[length(b)]])) #XXX we had no FALSE && here. Why did we want that?
                         substitute(return(x), list(x = b[[length(b)]]))
                       else
                         insertReturn(b[[length(b)]])
   } else {
      b = if(isSelect(b))
            substitute(return(x), list(x = b))
          else
            insertReturn(b)
   }

   # Add a return() if the last expression is  if() { ... } with no else
   # Do we really want to do this?
   last = b[[length(b)]]
   if(is.call(last) && as.character(last[[1]]) == "if" && length(last) == 3)
      b[[ length(b) + 1L ]] = quote(return( ))
   
   body(expr) = b
   expr
}
