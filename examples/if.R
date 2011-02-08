fi.simple =
function()
{
   if( 1L < 3L)
      return(10L)
}

fi.return =
  #
  # This version returns a value explicitly
  # We define a variable x with the correct type and 
  # then assign it a value
function()
{
  x = 0L
  if( 1L > 3L) {
     x = 100L
  } else if(4L < 1L) {
     x = 200L
  } else
     x = 300L
  return(x)
}



fi =
  #
  # This returns a value from the if () ....  "function"
  # There is no intermediate variable and no explicit return.
  # We can rewrite this before compiling or recognize this in the compiler
  #
function()
{
  if( 1L > 3L) {
     100L
  } else if(4L < 1L) {
     200L
  } else
     300L
}
