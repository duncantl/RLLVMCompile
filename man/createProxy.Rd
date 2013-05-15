\name{createProxy}
\alias{createProxy}
\title{Create an R function to call a native routine}
\description{
  This is a simple function that generates a compiled
  routine that we can call to invoke the native routine.
  We can already do this with \code{rdyncall}
  or \code{Rffi}.  However, this is another
  approach that uses LLVM.

  It is not clear we actually need this.
  We can call existing routines directly (except with ...) via .llvm.
  See the example on the Function page.
}
\usage{
createProxy(name, returnType, types = list(), id = sprintf("r\%s", name),
             mod = Module(name), ee = ExecutionEngine(mod),
              env = globalenv(), sym = getNativeSymbolInfo(name)$address, ...)
}
\arguments{
  \item{name}{the name of the routine to which we are interfacing}
  \item{returnType}{the return type of the routine to which we are interfacing}
  \item{types}{the parameter types of the target routine}
  \item{id}{the name of the R routine }
  \item{mod}{the LLVM module in which to create the routine}
  \item{ee}{the LLVM execution engine}
  \item{env}{the environment for the R function}
  \item{sym}{the address of the routine to which we are interfacing}
  \item{\dots}{additional arguments}
}
\value{
  The compiled routine that calls the native routine.
}
\author{
Duncan Temple Lang
}

\seealso{
\code{\link[Rllvm]{.llvm}}
}
%\examples{}
\keyword{programming}
