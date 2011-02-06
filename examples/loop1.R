# Here we loop over 2 parallel vector
# We mutate the existing x and in our script
# we do not need to copy it
# Same in R.
f = 
function(x) {
  for(i in seq(along = x)) {
     x[i] = 2 * x[i] + log(y[i]) 
  }
}

if(FALSE) {
  e = body(f)[[2]]
  var = e[[2]]
  thru = e[[3]]
  it = e[[4]][2]] # it for iterate
  lhs = it[[2]]
  rhs = it[[3]]
}
