
Dist =
  #
  # Compute the distance between two sets of observations
  #
  # Compile for numeric and integer inputs. 
  #  (Combinations of both?)
  #
  #
function(g1, g2, op = euclidean, ...)
{
  ans <- matrix(0, nrow(g1), ncol(g2))

  for(i in seq(length = nrow(g1)))
    for(j in seq(length = i))
       ans[i,j] <- ans[j,i] <- op(g1[i,], g2[j,], ...)

  ans
}

  
