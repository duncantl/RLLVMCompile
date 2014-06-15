# motivated by examples in the Riposte paper
f = function(data)
{
  x = log(data)
  mean = mean(x)
  sd = sd(x)
  return(c(mean = mean, sd = sd)) # explicitly identifying x as unreachable.
                                  # code depends could do this for us
}



