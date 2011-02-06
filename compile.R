## compile.R - compile R functions
# Some of this is inspired by Tierney's compiler package.

library(Rllvm)


## Test functions ##
add <- function(x, y) {
  xy.sum <- x+y # local var
  return(xy.sum)
}
## mod <- Module('testAdd')
## fun <- Function('Add', DoubleType, c(x=DoubleType, y=DoubleType), mod)
## block <- Block(fun)
## ir <- IRBuilder(block)
## params <- getParameters(fun)
## xy.sum <- createLocalVariable(ir, DoubleType, 'xysum')
## createStore(ir, xy.sum, 
## tmp = binOp(ir, Add, params$x, params$y)
## createReturn(ir, tmp)
## verifyModule(mod)

dumbAssign <- function() {
  x <- 3L
  return(x)
}
  
## Compiler ##
findVar <- function(var, env) {
  return(mget(var, envir=env, ifnotfound=NA))
}

# Make this a generic and have this as the default.
# Allow other people to provide their own.
findCall <- function(call) {
  op <- match(as.character(call), names(OPS))
  if (is.na(op))
    return(NA) # TODO Some (specified) builtins need to be passed directly,
               # i.e. as.double()
  return(OPS[[op]])
}

checkArgs <- function(args, types, fun) {
  # for side-effect of stop() when wrong arg type
  # encountered. TypeInfo will replace this probably.
  if (length(args) != length(types))
    stop("Wrong number of args provided.")
  for (i in seq_along(args)) {
    if (types[[i]] != 'ANY' && !(typeof(args[[i]]) %in% types[[i]]))
      stop(fun, " received an argument of the wrong type\n",
           "expected any of:\n", paste(types[[i]], sep=', ', collapse=', '),
           "\ngot: ", typeof(args[[i]]))
  }
}

getArgs <- function(expr) {
  # Converts to list
  if (typeof(expr) != "language")
    stop("expr must be of type 'language' in getArgs")
  return(sapply(expr[-1], function(x) x))
}

isNumericConstant <- function(expr) {
  # TODO no complex cases yet
  if (class(expr) %in% c('numeric', 'integer'))
    return(TRUE)
  return(FALSE)
}

assignHandler =
function(args, env, ir, nextBlock = NULL) {
          # CreateLocalVariable, with a reference in the
          # environment, and then CreateStore of the value.
   checkArgs(args, list(c('character', 'symbol'), 'ANY'), '<-')
   var <- as.character(args[1])
   val <- args[[2]]
   if (is.na(findVar(var, env))) {
     # Create new local store, TODO remove the type here and infer it
     cat("createLocalVariable for '", var, "'\n", sep='')
     assign(var, createLocalVariable(ir, DoubleType, var), envir=env) ## Todo fix type
   }
   ref <- get(var, envir=env)

   # Now, create store. TODO: how does this work *without*
   # constants? Where is evaluation handled... probably not
   # here?
   cat("createStore for '", var, "'\n", sep='')
   cat("object:\n"); print(val); cat("\n")
   createStore(ir, val, ref)
 }

addHandler =
function(args, env, ir, ...)  
{
  e = lapply(args, function(x)
                       if(is(x, "numeric"))
                         x
                       else {
                          ir$createLoad(get(as.character(x), env))
                       })
  
  ir$binOp(FAdd, e[[1]], e[[2]])
}

## Builtin Types to overload
OPS <- list('for' = compileForLoop,
            '<-' = assignHandler,
            '=' = assignHandler,            
            'return'=function(args, env, ir, nextBlock = NULL) {
              if (is.null(findVar('ReturnType', env)))
                stop("returnType must be in environment.")
              rt <- get('returnType', envir=env)
              # TODO check types -- how?
              checkArgs(args, list('ANY'), 'return')
              cat("createReturn for '", args[[1]], "'\n", sep='')
              cat("object:\n"); print(findVar(args[[1]], env)[[1]]); cat("\n")
              
              createReturn(ir, createLoad(ir, findVar(args[[1]], env)[[1]]))
            },
            '{' = function(args, env, ir, nextBlock = NULL) return(args), # TODO this doesn't work.
            '+' = addHandler
            )

compileExpressions =
  #
  # This compiles a group of expressions.
  # It handles moving from block to block with a block for
  # each expression.
function(exprs, env, ir, fun = NULL, name = getName(fun))
{
  for (i in seq_along(exprs)) {
    if (length(exprs) == 1)
      e <- exprs
    else
      e <- exprs[[i]]

    compile(e, env, ir, fun, name) 
  }    
}

compile <-
function(e, env, ir, fun = NULL, name = getName(fun), nextBlock = NULL)
{
#    cat("current expression: ", as.character(e), "\n")
    
    if (is.call(e)) {
      # Recursively compile arguments
      call.op <- findCall(e[[1]])
      if (typeof(call.op) != "closure" && is.na(call.op))
        stop("Cannot compile function '", e[[1]], "'")

      if(as.character(e[[1]]) == "for")
         call.op(e, env, ir, nextBlock = nextBlock)
      else {
         call.args <- list(lapply(getArgs(e),
                                    function(x) compile(x, env, ir)),
                           env, ir,
                           nextBlock = nextBlock)
         do.call(call.op, call.args)
      }
    } else if (is.symbol(e)) {
      var <- as.character(e)
      return(var) ## TODO: lookup here, or in OP function?
    } else if (isNumericConstant(e)) {
      cat("createContant for '", e, "'\n", sep='') # TODO when to use?
      return(as.numeric(e))
    }
}


compileFunction <-
function(fun, returnType, types = list(), mod = Module(name), name = NULL)
{
  ftype <- typeof(fun)
  if (ftype == "closure") {

    args <- formals(fun) # for checking against types; TODO
    fbody <- body(fun)

    if(length(names(types)) == 0)
      names(types) = names(args)
    
    # Find the name of the function if not provided
    if (is.null(name))
      name <- deparse(substitute(fun))

    # Grab types, including return. Set up Function, block, and params.
    argTypes <- types
    llvm.fun <- Function(name, returnType, argTypes, mod)
    block <- Block(llvm.fun, "entry")
    params <- getParameters(llvm.fun)  # TODO need to load these into nenv
    ir <- IRBuilder(block)
    
    nenv <- new.env( parent = emptyenv())
    nenv$.fun = llvm.fun
    nenv$.params = params
    nenv$.types = types
    
    # store returnType for use with OP$`return`
    assign('returnType', returnType, envir=nenv)    # probably want . preceeding the name.

       # For side effects of building mod
       # Have to be careful with one line functions with no {
    bdy = if(is(fbody, "{")) fbody[-1] else  fbody

    compileExpressions(bdy, nenv, ir, llvm.fun, name)    # TODO class { should be handled
                                              # better.  Can just do it as a separate block.
    return(list(mod=mod, fun=llvm.fun))
    
  } else
  stop("compileFunction can only handle closures")
}

## mod <- Module('testDumbAssign')
## fun <- Function('dumbAssign', Int32PtrType, c(), mod)
## block <- Block(fun)
## ir <- IRBuilder(block)
## #e <- new.env()
## #a <- OPS$`<-`(getArgs(body(dumbAssign)[[2]]), e, ir)
## #createReturn(ir, get('x', envir=e))
## x <- createLocalVariable(ir, Int32Type, "x")
## createStore(ir, createLoad(ir, createConstant(ir, 3L)), x)
## createReturn(ir, x)
## verifyModule(mod)


##
if(FALSE) {
mod <- Module('testDumbAssign')
m <- compileFunction(dumbAssign, mod, types=c(returnType=DoubleType))
verifyModule(m$mod)  # compileFunction should take care of this.
run(m$fun)

Optimize(m$fun)
}
