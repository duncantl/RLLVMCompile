\name{vectorizeFunction}
\alias{vectorizeFunction}
\title{Create a vectorized version of a scalar function}
\description{
  This function takes an R scalar function or a compiled version
  of an R function and generates a compiled version of a new function
  that loops over its inputs element at a time
  and calls the original scalar function.
  This is merely a compilation of a call to \code{sapply}.
}
\usage{
vectorizeFunction(f, fc = NULL, typeInfo = NULL, module = as(fc, "Module"),
                  scalarFunName = getName(fc), vectorArgName = names(parms)[1])
}
\arguments{
  \item{f}{the scalar R function}
  \item{fc}{the compiled version of this scalar function}
  \item{typeInfo}{information about the types of the scalar
    routine/function}
  \item{module}{the LLVM module in which to create the new routine}
  \item{scalarFunName}{the name of the compiled scalar routine}
  \item{vectorArgName}{the name of the parameter over which we want to
    vectorize the computations}
}
\value{
  The compiled routine
}
\author{
Duncan Temple Lang
}

\seealso{
\code{\link{compileFunction}}
}
\examples{

Dnorm = function(x, mu = 0, sd = 1)
         1/sqrt(2 * pi * sd^2) * exp( - .5*( (x-mu)/sd ) ^2)
fc = compileFunction(Dnorm, DoubleType, list(DoubleType, DoubleType,
         DoubleType))

g = vectorizeFunction(Dnorm, fc) 
}
\keyword{progamming}
