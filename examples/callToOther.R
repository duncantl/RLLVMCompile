# An example of being able to compile both of these in the
# same module and calling foo from bar.
foo =
function(x, y)
{
  return( x + y )
}

bar =
function(x, y)
{
  return ( foo(x, y) + 10 )  
}

foobar =
function(x, y)
{
  return ( sqrt(foo(x, y)) )
}
