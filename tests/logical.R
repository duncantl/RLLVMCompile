# logical.R - test compilation of logical operators and functions

t1 <- function(x) {
  return(!x)
}

t1.c <- compileFunction(t1, Int32Type, list(x=Int32Type), asList=TRUE, optimize=FALSE)
stopifnot(verifyModule(t1.c$mod))
