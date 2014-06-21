\name{getBuiltInRoutines}
\alias{getBuiltInRoutines}
\title{Specify list of Available Compiled Routines}
\description{
  \code{getBuiltInRoutines} is a function that returns
  a list of existing, compiled routines that are available
  for use by generated code using \code{RLLVMCompile}.
  Each routine has information about its return type and
  its parameter types.
  There is an existing collection of routines.
  One can also specify additional routines or override
  the details of existing ones in a call to \code{getBuiltInRoutines}.
}
\usage{
getBuiltInRoutines(..., env = NULL, useFloat = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{name=value pairs of function names and descriptions.
    Each element can be either an LLVM \code{Function} object 
    or a list specifying the return type and the parameter types.}
  \item{env}{an environment in which to find an existing computed list
    of routine descrptions stored in the variable
    \code{.builtInRoutines}.   This is used to generate and compute this list just once.}
  \item{useFloat}{a logical value. If this is \code{TRUE}, the basic
                  routines are defined to use \code{float} rather than
           \code{double}. This is useful for generating code for GPUs.}
}
\value{
A list.
}
%\references{}
\author{Duncan Temple Lang}

\seealso{\code{\link{compileFunction}}}
\examples{
 rr = getBuiltInRoutines()
 names(rr)

 rr = getBuiltInRoutines(foo = list(Int32Type))
 names(rr)
}
\keyword{programming}
\keyword{interface}
