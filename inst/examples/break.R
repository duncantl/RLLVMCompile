break.simple =
function()
{
  for(i in 1:10) {
     break
  }

  return(TRUE)
}

break.ctr =
function()
{
  ctr = 4L
  for(i in 1:10) {
     ctr = ctr + 1L
     if(ctr == 7L)
       break
  }

  return(ctr)
}



foo =
function()
{
  for(i in 1:10) {
     for(j in 1:10) {
       if(i == j)
         next
       ctr = ctr + 1
       if(ctr == 10)
         break
     }
  }
}
