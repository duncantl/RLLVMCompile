library(RLLVMCompile)
fib =
function(n)
{
  if(n == 0L || n == 1L)
     n
  else
     fib(n - 1L) + fib(n - 2L)
}


g1 =
function(x)
{
   x = sin(x)
}

g2 =
function(x)
{
   x = sin(x)
   x + 10
}


g3 =
function(x)
{
   x = sin(x)
   return(x + 10)
}


g4 =
function(x)
{
   return(x <- sin(x))
}

g5 =
function(x)
{
  x == 10
}

g6 = function(x, mu, sigma)
{
   total = 0
   for(val in x)
      total = total + log(Dnorm(val, mu, sigma))
   total
}

g7 <- Dnorm <-
function(x, mu, sigma)
{
   ( 1.0/(sqrt(2 * pi) * sigma)) * exp( - .5 * ((x - mu)/sigma)^2)
}

g8 = function(l) !l

