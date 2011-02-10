
Sum =
function(x)
{
   total = 0
   for(i in x)
      total = total + i

   return(total)
}

Sum1 = 
function(x)
{
   total = 0
   for(i in 1:1000000)
      total = total + 1

   return(total)
}

Sum2 =
  # See count.Rdb for analysis of the speedup.
function(len)
{
   total = 0
   for(i in 1:len)
      total = total + 1

   return(total)
}

Sum3 =
  # See count.Rdb for analysis of the speedup.
function(len)
{
   total = 0
   i = 0
  while(i < len) {
     i = i + 1
     total = total + 1
  }

   return(total)
}
