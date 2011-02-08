## compile.R - compile R functions
# Some of this is inspired by Tierney's compiler package.

assignHandler =
function(call, env, ir)
{
   args = call[-1]

   val = compile(args[[2]], env, ir)

          # CreateLocalVariable, with a reference in the
          # environment, and then CreateStore of the value.
   checkArgs(args, list(c('character', 'symbol'), 'ANY'), '<-')
     # XXX may not be a variable
   if(is.name(var))
      var <- as.character(args[1])
   
   val <- args[[2]]
   if (is.na(findVar(var, env))) {
     # Create new local store, TODO remove the type here and infer it
     type = getType(val, env)
     assign(var, createLocalVariable(ir, type, var), envir=env) ## Todo fix type
     env$.types[[var]] = type
   }
   
   ref <- get(var, envir = env)

      # Now, create store. TODO: how does this work *without*
      # constants? Where is evaluation handled... probably not
      # here?
   createStore(ir, val, ref)
 }


assignHandler =
  # Second version here so I don't mess the other one up.
function(call, env, ir, ...)
{
   args = call[-1]  # drop the =
   val = compile(args[[2]], env, ir)
   if(is.name(args[[1]])) {
      var = as.character(args[[1]])
      ref <- getVariable(var, env, ir, load = FALSE)
      if(is.null(ref)) {
         type = getType(val, env)
         assign(var, ref <- createLocalVariable(ir, type, var), envir=env) ## Todo fix type and put into env$.types
         env$.types[[var]] = type
       }
   } else {
      ref = compile(args[[1]], env, ir, ..., load = FALSE)
   }
   
   ans = ir$createStore(val, ref)
   ans
}






compile <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun))
   UseMethod("compile")


`compile.{` = compileExpressions =
  #
  # This compiles a group of expressions.
  # It handles moving from block to block with a block for
  # each expression.
function(exprs, env, ir, fun = env$.fun, name = getName(fun))
{
  if(as.character(exprs[[1]]) != "{")
      compile(exprs, env, ir, fun = fun, name = name)
  else {
    exprs = exprs[-1]
    for (i in seq_along(exprs)) {
        cur = ir$getInsertBlock()
        if(length(getTerminator(cur))) {
           browser()
            break
         }
        compile(exprs[[i]], env, ir, fun = fun, name = name)
    }
  }
}


compile.name <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun))  
{
   getVariable(e, env, ir, ...)
}

compile.integer <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun))  
{
   if(length(e) == 1)
      createIntegerConstant(e)
   else
     stop("not compiling integer vector for now")
}

compile.logical <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun))  
{
 # compile(as(e, "integer"), env, ir, ..., fun = fun, name = name)
  createLogicalConstant(e)
}


compile.numeric <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun))  
{
  if(length(e) == 1)
     createDoubleConstant(e)
  else
     stop("not compiling numeric vector for now")
}


compile.Value <-
  # This is just an LLVM value
function(e, env, ir, ..., fun = env$.fun, name = getName(fun))  
  e


compile.default <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun))  
{
    if (is.call(e)) {
         # Recursively compile arguments
      call.op <- findCall(e[[1]])
      if (typeof(call.op) != "closure" && is.na(call.op)) {
        # stop("Cannot compile function '", e[[1]], "'")
        call.op = findCall("call")
        direct = TRUE
      } else
        direct = FALSE

      if(direct ||
          (as.character(e[[1]]) %in% c("for", "while", "<", "if", "[", "[<-", MathOps, "=", "<-", LogicOps, "break", "next",
                                      "repeat", "call")))
         call.op(e, env, ir, ...)
      else {
            # I think we might want to just pass e and let the op function get the arguments if it wants them.
         call.args <- list(lapply(getArgs(e),
                                    function(x) compile(x, env, ir)),
                           env, ir)
                           
         do.call(call.op, call.args)
      }
    } else if (is.symbol(e)) {
      var <- as.character(e)
      return(var) ## TODO: lookup here, or in OP function?
    } else if (isNumericConstant(e)) {
      cat("createContant for '", e, "'\n", sep='') # TODO when to use?
      return(as.numeric(e))  # that's not an llvm object !?
    } else
      stop("can't compile objects of class ", class(e))
}



compileFunction <-
function(fun, returnType, types = list(), mod = Module(name), name = NULL,
          ..., .externalFunctionInfo = list(...))
{
  ftype <- typeof(fun)
  if (ftype == "closure") {

     .typeInfo = attr(fun, "llvmTypes")
     if( (missing(returnType) || missing(types)) && !is.null(.typeInfo)) {
       if(missing(types))
          types = .typeInfo$parms
       if(missing(returnType))
          returnType = .typeInfo$returnType
     }
     

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

    if(length(.externalFunctionInfo))
        processExternalFunctions(mod, .funcs = .externalFunctionInfo)

    globals = findGlobals(fun, merge = FALSE)
    compileCalledFuncs(globals, mod)
    
    block <- Block(llvm.fun, "entry")
    params <- getParameters(llvm.fun)  # TODO need to load these into nenv
    ir <- IRBuilder(block)

    nenv = makeCompileEnv()

    nenv$.fun = llvm.fun
    nenv$.params = params
    nenv$.types = types
    nenv$.module = mod
    
        # store returnType for use with OP$`return`
    assign('returnType', returnType, envir = nenv)    # probably want . preceeding the name.

    compileExpressions(fbody, nenv, ir, llvm.fun, name)

     
#   return(list(mod=mod, fun=llvm.fun, env = nenv))
    llvm.fun
    
  } else
     stop("compileFunction can only handle closures")
}

makeCompileEnv =
function()
{
   nenv <- new.env( parent = emptyenv())
   nenv$.continueBlock = list()
   nenv$.nextBlock = list()   
   nenv
}


processExternalFunctions =
function(mod, ..., .funcs = list(...), .lookup = TRUE)
{
  ans = mapply(declareFunction, .funcs, names(.funcs), MoreArgs = list(mod))

  if(.lookup) {
    syms = lapply(names(.funcs),
                    function(x) getNativeSymbolInfo(x)$address)
    llvmAddSymbol(.syms = structure( syms, names = names(.funcs)))
  }
  ans
}

declareFunction =
function(def, name, mod)
{
   # For now, just treat the def as list of returnType, parm1, parm2, ...
   # But want to handle if they split them into separate elements, e.g
   # list(name = "bob", returnType = .., parms = ...)
  ret = def[[1]]
  parms = def[-1]
  fun = Function(name, ret, parms, module = mod)
  setLinkage(fun, ExternalLinkage)
  fun
}


ExcludeCompileFuncs = c("{", "sqrt", "return", "+") # for now

compileCalledFuncs =
function(globalInfo, mod)
{
browser()
  funs = setdiff(globalInfo$functions, ExcludeCompileFuncs)

     # Skip the ones we already have in the module.
     # Possibly have different types!
  funs = funs[!(funs %in% names(getModuleFunctions(mod))) ]
  
  funs = structure(lapply(funs, get), names = funs)


  lapply(names(funs),  function(id) compileFunction(funs[[id]], mod = mod, name = id))
}
