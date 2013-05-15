\name{getCompilerHandlers}
\alias{getCompilerHandlers}
\title{Retrieve list of compiler handler functions}
\description{
  This function returns a collection of functions
  which are used to compile different
  parts of R code. This allows R programmers
  to merge their own handler functions with
  the defaults to customize how the compilation
  is done.
}
\usage{
getCompilerHandlers(...)
}
\arguments{
  \item{\dots}{a collection of name=function pairs where the function is
  the new handler and the name identifies its role.}
}
\value{
  A list of functions.
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{compileFunction}}
}
\examples{
 getCompilerHandlers( `call` = function(expr, env, ir, ...) { browser() } )
}
\keyword{programming}

