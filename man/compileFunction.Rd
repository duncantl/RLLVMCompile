\name{compileFunction}
\alias{compileFunction}
\title{Compile an R function to native machine code via LLVM}
\description{
  This is the main function of this package and takes an R function
  and attempts to compile the R code into a native routine.
  It does this by translating the R code to LLVM instructions
  and then compiling that LLVM description to machine code.
  This has the potential to greatly speed up the execution
  of simple R code that is not vectorized but works on elements
  of vectors separately.

  To compile the function, we need information about the types of the
  parameters and the return value of the function. These can be
  specified via \code{returnType} and \code{types}. Alternatively, if
  the function has an attribute \code{llvmTypes}, we get the type
  information from that.  This allows authors of the R function to
  provide information that can be used directly to compile the function.
  We plan to also support the \code{TypeInfo} package's annotation of
  functions with type information.

  This function attempts to compile other R functions that this
  function calls.  To do this, we need the type information.
  The signatures can be specified in the call via \code{.functionInfo}
  as a named list with an element for each function.
  The form of the element is a list with an element named returnType
  and another named params.

  Some of the functions this R function calls are already available in C
  libraries and we might want to use those directly, e.g. sqrt.
  To do this, we need to specify the name of the routine and
  its signature. We do this via the \code{.routineInfo}
  argument. This is a named list with the names identifying the
  routines.
  Each element is of the form returnType and params.
}
\usage{
compileFunction(fun, returnType, types = list(), mod = Module(name),
                name = NULL, asFunction = FALSE, optimize = TRUE, ...,
                .functionInfo = list(...), .routineInfo = list(),
               .compilerHandlers = OPS)
}
\arguments{
  \item{fun}{the R function to be compiled.}
  \item{returnType}{the LLVM type of the return value. This can be
    omitted if the function has an \code{llvmType} attribute giving the
    details of the signature.
  }
  \item{types}{a list giving the LLVM types for each of the parameters
    in the R function. As with \code{returnType}, this need not be
    specified if the function has an \code{llvmType} attribute.  Also, this
    can be omitted if the function has no parameters.
  }
  \item{mod}{the LLVM \code{Module} object.}
  \item{name}{the name to use for the function, typically obtained from
    deparsing \code{fun}, but which can be specified explicitly if
    not referencing the function directly in the call.
   }
  \item{asFunction}{a logical value}
  \item{\dots}{name = value pairs of type information describing other R
    functions that \code{fun} may call. This allows those to be compiled
    at the same time. These elements are collected into \code{.functionInfo}}
  \item{.functionInfo}{a list of named elements that provide the return
    type and parameter types of R functions. See \dots.}
  \item{.routineInfo}{a named list of signatures describing native
    routines that are to be considered callable from our compiled
    function and which that code may call.}
  \item{optimize}{a logical value that controls whether the module is
    optimized before being returned. This has the potential to make the
    functions faster.
  }
  \item{.compilerHandlers}{currently ignored, but intended to be a means
  to customizing the collection of specific handler functions for
  different language elements, i.e. to replace \code{OPS} in the package.}
}
\details{
}
\value{
 If \code{asFunction}, this returns an R function that has the same
 signature  as \code{fun} and which can be called identically
 but which will use the newly compiled function.
}
\references{
  The Rllvm  package on \url{www.omegahat.org} and
  LLVM itself at \url{llvm.org}.
}
\author{
Vince Buffalo and Duncan Temple Lang
}

\seealso{
  The compile methods in the package and the \code{OPS}
  list of handler functions that control which functions get called to
  compile the different language elements, e.g. if, while, for, calls,
  +, ....
}
\examples{

    # An example of being able to compile both of these in the
    # same module and calling foo from bar.
foo =
function(x, y)
{
  return( x + y )
}

bar =
function(x, y)
{
  return ( foo(x, y) + 10 )  
}

foobar =
function(x, y)
{
  return ( sqrt(foo(x, y)) )
}


 fb = compileFunction(foobar, DoubleType, list(DoubleType, DoubleType),
                      .routineInfo = list( sqrt = list(DoubleType, DoubleType) ),
                       foo = list(returnType = DoubleType,
                                  params = c(DoubleType, DoubleType)))

 run(fb, 4, 5) # gives 3


  # Here we return an R function that is directly callable.
 fb = compileFunction(foobar, DoubleType, list(DoubleType, DoubleType),
                      .routineInfo = list( sqrt = list(DoubleType, DoubleType) ),
                       foo = list(returnType = DoubleType,
                                  params = c(DoubleType, DoubleType)),
                      asFunction = TRUE)

 fb(4, 5)

}
\keyword{programming}
\concept{compilation}
\concept{high performance computing}
