g =
function(n)
{
  ans = c()
  for(i in rnorm(n)) {
       ans[i] = if(abs(i) > 3) log(i) else i
  }
