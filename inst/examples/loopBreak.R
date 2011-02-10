loop.break =
function()
{
  for(i in 1:10) {
    if( i == 5L )
       break
  }
  return(i)
}

loop.next =
function()
{
  ctr = 0L
  for(i in 1:10) {
    printi(i)
    if( i == 5L )
       next
    ctr = ctr + 1L
  }
  return(ctr)
}
