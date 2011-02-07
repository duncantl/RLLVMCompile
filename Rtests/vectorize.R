ex.wrong =
function(l, mu = 2, sd = 3) {
  x <- numeric(l)
  x[1] <- rnorm(1, mu, sd)
  for (i in 2:l) {
    x[i] <- x[i-1] + rnorm(1, mu, sd)
  }
  return(x)
}

ex.right =
function(l, mu = 2, sd = 3) {
  x <- numeric(l)
  r <- rnorm(l, mu, sd)
  x[1] <- r[1]
  for (i in 2:l) {
    x[i] <- x[i-1] + r[i]
  }
  return(x)
}


set.seed(0)
system.time(x.wrong <- ex.wrong(10000))

set.seed(0)
system.time(x.right <- ex.right(10000))

stopifnot(x.right == x.wrong)


checkVectorizedCodeMotion(body(ex.wrong)[[4]][[4]], names(formals(ex.wrong)),
                          quote(len), quote(i))
