#
# The idea here is that we can be more efficient with memory
# in a bootstrap loop.
# We sample with replacement from rows of a data frame (or elements of a vector)
# We will reuse the memory for the data frame across each iteration
# Similarly, we can avoid allocating the vector of sample indices
# How much will this speed things up?
#

replicate(B, {
               d.star = data[sample(1:n, n, replace = TRUE), ]
               T(d.star, ...)
             })

# or
f = function(data, B = 100, n = nrow(data))
{
  ans = numeric(B)
  for(i in 1:B) {
    d.star = data[sample(1:n, n, replace = TRUE), ]
    ans[[i]] = T(d.star, ...)
  }
}

