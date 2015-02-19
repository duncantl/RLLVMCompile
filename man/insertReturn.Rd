\name{insertReturn}
\alias{insertReturn}
\title{Insert explicit calls to \code{return}}
\description{
  This function modifies an R function or expression
  to insert explicit calls to \code{return}
  in appropriate places in the expressions.
}
\usage{
insertReturn(expr, nested = FALSE, ...)
}
\arguments{
  \item{expr}{the function or expression to be potentially modified}
  \item{nested}{a logical value   that controls whether we explicitly
    wrap a call within a \code{return} call.}
  \item{\dots}{additional parameters for methods such as the compiler object}
}
\value{
 The original object \code{expr} which is potentially modified.
}
\author{DTL}
\seealso{\code{\link{compileFunction}}}
\examples{
f = function(x)  x + 1
insertReturn(f)

f = function(x) {
      if(x < 0)
         -x
      else
         10* xn
    }

insertReturn(f)
}
\keyword{programming}

