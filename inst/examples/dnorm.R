dnorm =
function(x, mean = 0, sd = 1)
{
   return( 1/(sqrt(pi* 2 * sd ^2)) * exp(- (x - mean)^2/(sd^2 * 2)) )
}
