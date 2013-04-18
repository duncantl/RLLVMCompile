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
function(expr, nested = FALSE, ...)
  UseMethod("insertReturn")


`insertReturn.{` =
function(expr, nested = FALSE, ...)
{
     expr[[length(expr)]] = insertReturn(expr[[length(expr)]], nested)
     expr
}

insertReturn.name =
function(expr, nested = FALSE, ...)
{
   substitute(return(x), list(x = expr))
}


`insertReturn.call` =
function(expr, nested = FALSE, ...)
{
  if(nested || expr[[1]] != as.name('return')) {
    if(FALSE && nested) {
        # create .ret = expr
      k = quote(.ret <- val)
      k[[3]] = expr
    } else {
      k = call('return')
      k[[2]] = expr
    }
    k
  } else
    expr
}

insertReturn.if =
function(expr, nested = FALSE, ...)
{
  expr[[3]] = insertReturn(expr[[3]], nested = TRUE)
  if(length(expr) == 4)
      expr[[4]] = insertReturn(expr[[4]], nested = TRUE)
  expr
}

insertReturn.numeric = insertReturn.logical = insertReturn.character =
  insertReturn.integer = `insertReturn.(` =     # should check for (return(x+1))
   function(expr, nested = FALSE, ...) {
     k = call('return')
     k[[2]] = expr
     k
   }


insertReturn.while =
function(expr, nested = FALSE, ...)
{
  expr[[3]] = insertReturn(expr[[3]], nested = TRUE)
  expr
}

`insertReturn.function` =
function(expr, nested = FALSE, ...)
{
#   body(expr) = insertReturn(body(expr))
   b = body(expr)
   if(class(b) == "{")
      b[[length(b)]] = insertReturn(b[[length(b)]])
   else
      b = insertReturn(b)
   
   body(expr) = b
   expr
}
