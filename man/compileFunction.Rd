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
compileFunction(fun, returnType, types = list(), module = Module(name), name = NULL,
         NAs = FALSE,
         asFunction = FALSE, asList = FALSE,
         optimize = TRUE, ...,
         .functionInfo = list(...),
         .routineInfo = list(),
         .compilerHandlers = getCompilerHandlers(),
         .globals = getGlobals(fun, names(.CallableRFunctions), .ignoreDefaultArgs, 
                                .assert = .assert, .debug = .debug),
         .insertReturn = !identical(returnType, VoidType),
         .builtInRoutines = getBuiltInRoutines(),
         .constants = getConstants(),
         .vectorize = character(), .execEngine = NULL,
         structInfo = list(), .ignoreDefaultArgs = TRUE, .useFloat = FALSE,
         .zeroBased = logical(), .localVarTypes = list(),
          .fixIfAssign = TRUE, .CallableRFunctions = list(), 
         .RGlobalVariables = character(),
         .debug = TRUE, .assert = TRUE, .addSymbolMetaData = TRUE,
         .readOnly = constInputs(fun), .integerLiterals = TRUE)
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
  \item{module}{the LLVM \code{Module} object.}
  \item{name}{the name to use for the function, typically obtained from
    deparsing \code{fun}, but which can be specified explicitly if
    not referencing the function directly in the call.
  }
  \item{NAs}{a logical controlling whether to add code to handle NAs in
  the computations. If we know that there are no NAs in the inputs, we
  can avoid adding extra code to handle them.  Currently ignored.}
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
  \item{.compilerHandlers}{a named list of functions.
    This is used to find a handler for generating
    code for different language constructs and for calls
    to particular R functions.
    Expressions are compiled using methods of the \code{compile}
    function. But not all expressions have a class that identifies
    their purpose, e.g. return which appears as a regular call.
    So in these cases, we use this named list to find an element
    for the particular call.  This allows the caller
    to customize how we generate code for a call to a particular
    R function.  The basic idea is that one makes a copy of the
    default handlers and replaces or adds entries to that and passes
    the modified list to \code{compileFunction}.
  }
  \item{.globals}{a character vector giving the names of functions
    which are to be compiled also in this module. These are typically
    the names of functions that are called within the body of this
    function.}
  \item{.execEngine}{the execution object which, if present and we are
  creating an R function to hide the compiled routine, is added as a
  default value of a parameter so that the caller doesn't have to
  specify it.}
  \item{structInfo}{a list with an element for each struct type we might
  reference in the code. Each element should be a named list giving the
  field names in the struct and their types.}
  \item{.builtInRoutines}{a list of available routines we know about
  along with their return type and parameter types, i.e. signature}
  \item{.insertReturn}{whether we need to add explicit calls to
    \code{return} in the R function before we compile it.
  If we know that the code already contains the \code{return} calls in
  the appropriate places, we can save time by not doing this, but it is
  very small and never hurts to do this.}
  \item{.constants}{the names of variables that are to be considered constants}
  \item{.vectorize}{a logical value controlling whether to make the code
  vectorized in its first argument, or if this is a parameter name then
  that parameter.}
  \item{asList}{a logical value that controls what is returned. If this
  is \code{TRUE}, we return a list with the module, the compiled routine
  and the compiler object containing all of its state}
  \item{.ignoreDefaultArgs}{a logical value which controls whether we
  look at the code  for the default values of the parameters in the R
  function. Currently we use this to avoid working with global variables
  in R that we will not actually reference.}
 \item{.useFloat}{a logical value. This controls whether we use double
   or float data type for numeric variables.}
 \item{.zeroBased}{logical value. This controls whether we use 0-based
  counting. This is needed for the compilation of code for the GPU (?)}
 \item{.localVarTypes}{a named list giving the type objects for any of
         the local variables. These are "hints". The types should be
  LLVM types, i.e. objects in the \code{Rllvm} package representing
  explicit types.}
\item{.fixIfAssign}{a logical value that controls if we process the
  R function code to rewrite assignments of the form \code{x = if(cond) a else b},
   to \code{if(cond) x = a else x = b} via the \code{\link{fixIfAssign}}
  function.}
\item{.CallableRFunctions}{a list with named elements. Each element
  identifies an R function that can be called from the compiled code.
  Each element is a list specifying the return type of the function and
  for the parameters of that function. These are specified as LLVM
  types.  The type information can also be specified within the R code
  via  \code{.R(foo(a, b), list(returnType, arg1Type, arg2Type))}.}
\item{.RGlobalVariables}{a character vector specifying the names of variables
  that are  in the R global environment. The types of these variables is
  computed at compile time if necessary.}
\item{.debug}{either a logical value or a character vector. If
  \code{TRUE}, calls in the R code of the form \code{.debug(...)} are
  compiled. If \code{FALSE}, they are omitted. If a character vector is
  provided, this allows the caller to specify other names for functions
  which are considered debugging calls.}
\item{.assert}{similar to \code{.debug} but for assertions. By default,
  these are of the form \code{.assert(condition)}}
\item{.addSymbolMetaData}{a logical value that controls whether meta
                           data about the external symbols invoked in
                           the compiled code are added to the
  Module. This allows another session/application to resolve those
  symbols correctly when the module is deserialized.}
%\item{.duplicateParams}{}
\item{.readOnly}{a character vector giving the names of any parameters
 in the compiled code that are pointer types but read-only and not
  written to.  The \code{constInputs} function in the
  \code{CodeAnalysis} package can determine this in common cases.
  However, one can avoid the cost of that analysis or provide the
  information that it cannot determine directly.}
\item{.integerLiterals}{a logical value. If this is \code{TRUE},
  assignments of the form \code{x = 2} will treat the numeric value
  as an integer. This is different from R which treats 2 as a
  \code{numeric} object evethough its value - not its type - is an integer.
 }
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

 foo.c = compileFunction(foo, DoubleType, list(DoubleType, DoubleType),
                      .routineInfo = list( sqrt = list(DoubleType, DoubleType) ))


 fb = compileFunction(foobar, DoubleType, list(DoubleType, DoubleType),
                       module = as(foo.c, "Module"),
                      .routineInfo = list( sqrt = list(DoubleType, DoubleType) ),
                       foo = list(returnType = DoubleType,
                                  params = c(DoubleType, DoubleType)))

 run(fb, 4, 5) # gives 3 = sqrt(4 + 5)
 .llvm(fb, 4, 5)


  # Here we return an R function that is directly callable.

 # Create a new module and we will need foo in that. So recreate.
 foo.c = compileFunction(foo, DoubleType, list(DoubleType, DoubleType),
                      .routineInfo = list( sqrt = list(DoubleType, DoubleType) ))
 fb = compileFunction(foobar, DoubleType, list(DoubleType, DoubleType),
                       module = as(foo.c, "Module"),
                      .routineInfo = list( sqrt = list(DoubleType, DoubleType) ),
                       foo = list(returnType = DoubleType,
                                  params = c(DoubleType, DoubleType)),
                      asFunction = TRUE)

 fb(4, 5)



 #  Here we show how to override how an expression is called
 # by providing our own .compilerHandlers. Here we change
 # any call to Sys.Date() to a constant.
myDate = function() {
        return(Sys.Date())
     }

myOPS = getCompilerHandlers()
myOPS[["Sys.Date"]] =
   function(call, env, ir, ...) {
       cat("In Sys.Date handler\n")
       ir$createConstant(15015L)
   }


  # Note we specify .functionInfo so that we don't
  # try to create a Function object in the module for Sys.Date which
  # we won't end up calling.
f = compileFunction(myDate, Int32Type, optimize = FALSE,
                     .compilerHandlers = myOPS, .globals = NULL)

.llvm(f)


myPlus = function() {
          return(1L + x)
     }
}
\keyword{programming}
\concept{compilation}
\concept{high performance computing}
