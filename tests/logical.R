# logical.R - test compilation of logical operators and functions

t1 <- function(x) {
  return(!x)
}

t1.c <- compileFunction(t1, Int32Type, list(x=Int32Type), asList=TRUE, optimize=FALSE)
stopifnot(verifyModule(t1.c$mod))

# This is to test some new type coercion for comparisons I've added.
t2 <- function(x) {
  a <- 0L ## we need this always. 
  if (x < 100L) {
    a <- 1L
  } else {
    a <- 2L
  }
  return(a)
}

t2.c <- compileFunction(t2, Int32Type, list(x=DoubleType), asList=TRUE, optimize=FALSE)
stopifnot(verifyModule(t2.c$mod)) 
