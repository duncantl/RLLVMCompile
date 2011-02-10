foo = 
function(x)
{
  stride = nrow(x)
  for(i in 1:nrow(x))
    for(j in 1:ncol(x)) {
      if(x[i * stride + j] == 3)
        ctr = ctr + 1
    }
}
