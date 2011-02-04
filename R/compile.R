## compile.R - compile R code into native LLVM code

library(Rllvm)
InitializeNativeTarget()

## Test functions ##
# Basic op detection
add <- function(x, y) {
  xy.sum <- x+y # local var
  return(xy.sum)
}

EVALRULES <- list('<-'=c(var=FALSE, val=TRUE),
                  '+'=c(x=TRUE, y=TRUE),
                  'return'=c(rtrn=TRUE))

OPS <- list(
            '<-'=function(var, val, other) {
              # Arg types:
              #  x=character, val=quoted expression, type=array of Rllvm types
              #  Grabbed from parent environment: ir=IRBuilder object, env=environment
              #
              # Assignment will create a local variable if there is no
              # such variable in the environment. Otherwise, it will
              # give this variable the new value.
              #
              # The argument 'type' should disappear at some point, and
              # be inferred
              #
              # 'val' must be evaluated, but var must be *not* evaluated. 
              subs <- list(x=var, ir=other$ir, type=DoubleType) ## TODO type is static for testing
              if (!(var %in% ls(envir=other$env))) {
                # Create local store; the eval(substitute()) non-sense
                # is to maybe allow future compiled code printing via
                # wrapping substitute.
                assign(var, eval(substitute(createLocalVariable(ir, type, var), subs)), envir=other$env)
              }
              # Now, store value
              substitute(createStore(ir, val, ptr), list(ir=other$ir, val=val, ptr=get(var, envir=other$eval)))
            },
            '+'=function(x, y, other) {
              # Grabbed from parent environment: ir=IRBuilder object, env=environment
              return(substitute(binOp(ir, Add, x, y), list=c(ir=other$ir, x=x, y=y)))
            },
            'return'=function(rtrn, other) {
              # Grabbed from parent environment: ir=IRBuilder object
              return(createReturn(other$ir, rtrn))
            }
            )


evalArgs <- function(args, fun, other) {
  #args.to.eval <- cb.args[!is.na(EVALRULES[[as.character(cb[[1]])]])] # only eval relevant args
  env <- other$env
  
  # Evaluate (some) args of a function, depending on EVALRULES.
  evald.args <- vector('list', length(args))
  rules <- EVALRULES[[fun]]

  # If no special rule, evaluate all.
  if (is.null(rules)) {
    #cat("no eval rules for: "); print(fun)
    return(args, function(a) eval(a, envir=env))
  }
      
  #cat("eval rules:\n"); print(rules)

  # naive positional argument matching *only*
  for (i in seq_along(args)) {
    print(rules[i])
    if (!is.na(rules[i]) & rules[i]) { ## TODO handle NA cause.
      #cat("evaluating argument: "); print(args[i])
      evald.args[[i]] <- eval(args[[i]], envir=env)
    } else {
      #cat("not evaluating argument: "); print(args[i])
      if (is.na(rules[i])) {
        evald.args[[i]] <- args[[i]]
      } else {
        #cat("symbol: "); print(as.character(as.symbol(args[[i]])))
        evald.args[[i]] <- as.character(as.symbol(args[[i]]))
      }
    }
  }
  evald.args[[i+1]] <- other
  #cat("evaluated arguments: "); print(evald.args)
  return(evald.args)
}

compileFun <- function(fun, mod, types, name=NULL) {
  stack <- list() # not used yet; maybe wrapper around substitute will
                  # push compiled instructions here for debugging.

  args <- formals(fun) # for checking against types; TODO
  fbody <- body(fun)

  # Find the name of the function if not provided
  if (is.null(name))
    name <- deparse(substitute(fun))

  # Grab types, including return. Set up Function, block, and params.
  argTypes <- types[-c(which(names(types) == 'returnType'))]
  llvm.fun <- Function(name, types[['returnType']], argTypes, mod)
  block <- Block(llvm.fun)
  params <- getParameters(llvm.fun)
  ir <- IRBuilder(block)

  eval.env <- new.env()
  
  cmp <- function(code.blocks) {
    ## Loop over code blocks, recursively compiling each call.
    tmp <- sapply(code.blocks, function(cb) {
      cat("current code block: "); print(cb)
      if (is.call(cb)) {
        call.parts <- cmp(cb[-1])  # ignore the function part of the call for now

        # Find the matching function in OPS, otherwise error out.
        call.op <- match(as.character(cb[[1]]), names(OPS))
        if (is.na(call.op))
          stop("Operation '", as.character(cb[[1]]), "' not supported.\n")

        # *Very* naive argument matching
        cb.args <- c(call.parts)
        cb.call <- OPS[[call.op]]
        cat("Code block call: "); print(cb.call)

        other <- list(ir=ir, env=eval.env)
          browser()
        return(do.call(cb.call, evalArgs(cb.args, as.character(cb[[1]]), other), eval.env))
      } else {
        cat("non-call:", as.character(cb), "\n")

        # Search for variable in parameters first
        var <- params[[as.character(cb)]]
        if (is.null(var)) {
          # Not found in params; search for in local stores
          if (as.character(cb) %in% ls(envir=eval.env)) {
            return(get(as.character(cb), envir=eval.env))
          }
        } else {
          return(var)
        }
        return(as.symbol(as.character(cb)))
        # Could not find var anywhere else; error out
        #stop("Could not find object '", as.character(cb), "'\n")
      }
    })
  }
  
  return(cmp(fbody[-1]))
}

mod <- Module('testAdd')
compileFun(add, mod, c(returnType=DoubleType, x=DoubleType, y=DoubleType))
