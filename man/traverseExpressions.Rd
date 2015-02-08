\name{traverseExpressions}
\alias{traverseExpressions}

\title{General mechanism to process an R language object recursively and
process each call object}
\usage{
traverseExpressions(x, fun = function(x) x, ...)
}
\arguments{
  \item{x}{the R language object to process}
  \item{fun}{the function to invoke for each R \code{call} object in the
  tree}
  \item{\dots}{additional arguments passed to \code{fun} each time it is
  called}  
}  
\author{Duncan Temple Lang}