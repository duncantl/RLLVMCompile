library(RLLVMCompile)

# subset.R - test subset functions

## Note the return type is a DoubleType, so createCast dereferences
## the DoublePtrType - this is unsafe. More elaborate pre-compiling
## checking will handle this.
t1 <- function(x, i) {
  a <- x[i]
  a
}

t1.c <- compileFunction(t1, DoubleType, list(x = DoublePtrType, i = Int32Type), asList = TRUE, optimize = TRUE)
stopifnot(verifyModule(t1.c$mod))
stopifnot(run(t1.c$fun, c(1, 2, 3), 2L) == 2)


t2 <- function(x, i) {  
  b <- x[i] + 2.1
  b
}

t2.c <- compileFunction(t2, DoubleType, list(x = DoublePtrType, i = Int32Type), asList = TRUE, optimize = FALSE)
stopifnot(verifyModule(t2.c$mod))
stopifnot(run(t2.c$fun, c(1, 2, 3), 2L) == 4.1)



t3 <- function(x, i) {
  a <- 20
  b <- x[i] + a
  return(b)
}


t3.c <- compileFunction(t3, DoubleType, list(x=DoublePtrType, i=Int32Type), asList=TRUE, optimize=FALSE)
stopifnot(verifyModule(t3.c$mod))
stopifnot(run(t3.c$fun, c(1, 2, 3), 2L) == 22)


t4 <- function(x, i)
  x[i] + 20

t4.c <- compileFunction(t4, DoubleType, list(x = DoublePtrType, i = Int32Type), asList = TRUE, optimize = TRUE)
stopifnot(run(t4.c$fun, c(1, 2, 3), 2L) == 22)


#====================================================================
  
t5 <-
function(x, i) {
  x[i] <- 20
  x
}

t5.c <- compileFunction(t5, DoublePtrType, list(x = DoublePtrType, i = Int32Type))
.llvm(t5.c, c(1, 2, 3), 2)
# This returns a double*. We have to get the values.
# We should be able to write the code in R for this and compile it.
# e.g. function(src, n) {
#          ans = numeric(n)
#          for(i in 1:n)
#             ans[i] = src[i]
#          ans
#  }
# See copyArray.R in explorations/
