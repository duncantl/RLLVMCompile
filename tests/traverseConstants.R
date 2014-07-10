library(RLLVMCompile)

f =
function(x, mu, sd)
{
  ans = numeric(length(x))
  for(i in seq(along = x))
     ans[i] = 1/(sd * sqrt(2*pi)) * exp( - (x[i] - mu)^2/2*sd*sd)
  ans
}


replaceConst =
function()
{

}

traverseExpressions(f)
