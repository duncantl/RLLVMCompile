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

# Currently Rllvm is doing casts but not storing the results in an
# environment. If repeated casting is done on the same variable, does
# Rllvm optimize this away and create an intermediate variable?
t3 <- function(x) {
  a <- x + 3.1
  b <- x + 2.1
  c <- a + b
  return(c)
}

t3.c <- compileFunction(t3, DoubleType, list(x=Int32Type), asList=TRUE, optimize=FALSE)
stopifnot(verifyModule(t3.c$mod))


# Type coersion of return type
t4 <- function(x, y) {
  a <- x + y
  return(a)
}

t4.c <- compileFunction(t4, Int32Type, list(x=Int32Type, y=DoubleType), asList=TRUE, optimize=FALSE)
