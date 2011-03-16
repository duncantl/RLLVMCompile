# Here we loop over 2 parallel vector
# We mutate the existing x and in our script
# we do not need to copy it
# Same in R.

inplace =
function(x, len) {
  for(i in 2:len) 
     x[i] = 2 * i

  return(x)
}  

b1 =
  # Leaving len as a parameter so that we don't have to call
  # Rf_length() on x, especially since we have already unpeeled it from the SEXP
  # and so don't know the length
function(x, len) {
  for(i in 2:len) 
     x[i] = 2 * i

  return(x)
}

b =
  # Leaving len as a parameter so that we don't have to call
  # Rf_length() on x, especially since we have already unpeeled it from the SEXP
  # and so don't know the length
function(x, len) {
  for(i in 2L:len) {
     x[i] = 2L * x[i-1L]
  }
  return(x)
}


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
  it = e[[4]][[2]] # it for iterate
  lhs = it[[2]]
  rhs = it[[3]]
}
