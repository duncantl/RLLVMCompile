# assign.R - tests in compiling R assignment
#
# When these are more solified; make each test an element in a vector
# and loop over tests.

# simple constant assignment
t1 <- function(x) {
  y <- 4L
  xy.sum <- y + x
  return(xy.sum)
}

t1.c <- compileFunction(t1, Int32Type, list(x=Int32Type), asList=TRUE, optimize=FALSE)
stopifnot(verifyModule(t1.c$mod))

# simple variable reassignment; TODO broken
t2 <- function(x) {
  y <- 4L
  x <- y
  return(x)
}

t2.c <- compileFunction(t2, Int32Type, list(x=Int32Type), asList=TRUE, optimize=FALSE)
stopifnot(verifyModule(t2.c$mod))

