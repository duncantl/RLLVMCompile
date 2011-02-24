# math.R - basic maths functions

t1 <- function(x, y) {
  return(x + y)  
}

t1.c <- compileFunction(t1, DoubleType, list(x=DoubleType, y=DoubleType), asList=TRUE, optimize=FALSE)
stopifnot(verifyModule(t1.c$mod))


# Type mixing in arithmetic; should coerce to double using
# CreateSIntToFPInst
t2 <- function(x, y) {
  a <- x + y
  return(a)
}

t2.c <- compileFunction(t2, DoubleType, list(x=Int32Type, y=DoubleType), asList=TRUE, optimize=FALSE)
stopifnot(verifyModule(t2.c$mod))





