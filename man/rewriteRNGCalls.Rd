\name{rewriteRNGCalls}
\alias{rewriteRNGCalls}
\alias{fixIfAssign}

\title{Rewrite R language objects for Random Number Generation and
  assignments of an if statement}
\description{
}
\usage{
rewriteRNGCalls(x, ...)
fixIfAssign(expr, var = character(), ...) 
}
\arguments{
  \item{x,expr}{an expression/language object which is to be processed
    and potentially rewritten}
  \item{var}{the name of the variable being assigned}
  \item{\dots}{additional arguments for methods}
}
\value{
 A potentially modified language object.
}
\author{
Duncan Temple Lang
}

\seealso{traverseExpressions}
\examples{
}
\keyword{programming}
