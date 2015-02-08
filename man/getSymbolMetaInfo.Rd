\name{getSymbolInfoMetadata}
\alias{getSymbolInfoMetadata}

\title{Query the meta-data about native routines in an LLVM module}
\description{
  This function is used to retrieve the information about native
  symbols in an LLVM module so that we can load the relevant
  packages and DLLs and then register the routines  with
  an LLVM \code{\link[Rllvm]{ExecutionEngine}}.
}
\usage{
getSymbolInfoMetadata(module, id = character())
}
\arguments{
  \item{module}{the LLVM module to query.}
  \item{id}{a character vector giving the names of the routines.}
}
\author{Duncan Temple Lang}


\keyword{programming}