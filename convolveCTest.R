dyn.load("convolve.so")
conv <- function(a, b)
    .C("convolve",
       as.double(a),
       as.integer(length(a)),
       as.double(b),
       as.integer(length(b)),
       ab = double(length(a) + length(b) - 1))$ab

conv(1:10, 1:10)
