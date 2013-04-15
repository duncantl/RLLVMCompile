## compile.R - compile R functions
# Some of this is inspired by Tierney's compiler package.

MathOps = c("+", "-", "*", "/", "%/%", "^")
LogicOps = c("<", ">", "<=", ">=", "!=", "==", "!")

XXXX.assignHandler =
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
     type = getDataType(val, env)
     assign(var, createLocalVariable(ir, type, var), envir=env) ## Todo fix type
     env$.types[[var]] = type
   }
   
   ref <- get(var, envir = env)

      # Now, create store. TODO: how does this work *without*
      # constants? Where is evaluation handled... probably not
      # here?
   createStore(ir, val, ref)
 }


`compile.=` = `compile.<-`  = assignHandler =
  # Second version here so I don't mess the other one up.
function(call, env, ir, ...)
{
   args = call[-1]  # drop the = or <-

   if(isLiteral(args[[2]])) {
      val = eval(args[[2]])
      val = makeConstant(ir, val, getDataType(val))
   } else
      val = compile(args[[2]], env, ir)

   if(is.name(args[[1]])) {
      var = as.character(args[[1]])

      # We don't search parameters for the var name, since we don't
      # want to try to assign over a parameter name.
      ref <- getVariable(var, env, ir, load = FALSE, search.params=FALSE)
      if(is.null(ref)) {
        # No existing variable; detect type and create one.
        type = getDataType(val, env)
        if (is.null(type)) {
          # Variable not found in env or global environments; get type via Rllvm
          if (is(val, "StoreInst")) {
            # This is from the val = compile(); probably from a
            # statement like: y <- 4L; x <- y. When args[[2]] is
            # compiled above, getVariable returns an object of class
            # StoreInst. We ignore the current val, and instead query
            # the type from the variable.
            type = getDataType(args[[2]], env)
          }

          if (is(val, "Value"))
            type = getDataType(val)
        }
         assign(var, ref <- createLocalVariable(ir, type, var), envir=env) ## Todo fix type and put into env$.types
         env$.types[[var]] = type
       }
   } else {
      ref = compile(args[[1]], env, ir, ..., load = FALSE)
   }

   ans = ir$createStore(val, ref)
   val  # return value - I (Vince) changed this to val from ans. There seems
        # to be very little we can do with the object of class
        # StoreInst. Note: this seems to be the way it's done here
        # too: http://llvm.org/docs/tutorial/LangImpl7.html
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
  #insertReturn(exprs)
  if(as.character(exprs[[1]]) != "{")
      compile(exprs, env, ir, fun = fun, name = name)
  else {
    exprs = exprs[-1]
    idx = seq_along(exprs)
    for (i in idx) {
        cur = ir$getInsertBlock()
        if(length(getTerminator(cur))) {

            break
         }
         env$.remainingExpressions = exprs[ - (1:i) ]
         compile(exprs[[i]], env, ir, fun = fun, name = name)
#        # One approach to handling the lack of an explicit return is to
#        # create the return instruction ourselves, or to add a return
#        # around the call before we compile. The advantage of the latter
#        # is that any code generation that we write to ensure the correct
#        # return type on the expressions e.g. return(x + 1) will do the correct
#        # thing on the x + 1 part, not later converting the value.
#      if(i == idx[length(idx)] && !is.call(exprs[[i]]) || exprs[[i]][[1]] != as.name('return')) { # last one
#        ir$createReturn(val)
#      } else
#        val
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
    if(is(e, "Value") || is(e, "Instruction"))
      return(e)
    
    if (is.call(e)) {
           # Recursively compile arguments
      call.op <- findCall(e[[1]], env$.compilerHandlers)
      if (typeof(call.op) != "closure" && is.na(call.op)) 
        call.op = findCall("call", env$.compilerHandlers)

      call.op(e, env, ir, ...)

    } else if (is.symbol(e)) {
      var <- as.character(e)
      return(var) ## TODO: lookup here, or in OP function?
    } else
      stop("can't compile objects of class ", class(e))
}



compileFunction <-
function(fun, returnType, types = list(), mod = Module(name), name = NULL,
         NAs = FALSE,
         asFunction = FALSE, asList = FALSE,
         optimize = TRUE, ...,
         .functionInfo = list(...),
         .routineInfo = list(),
         .compilerHandlers = CompilerHandlers,
         .globals = findGlobals(fun, merge = FALSE),
         .insertReturn = TRUE,
         .builtInRoutines = getBuiltInRoutines(),
         .constants = getConstants(),
         .vectorize = character(), .execEngine = NULL)
{
  if (.insertReturn)
    fun = insertReturn(fun)

  if(!missing(types) && !is.list(types))
    types = structure(list(types), names = names(formals(fun))[1])
  
  #fun = fixIfAssign(fun)
  
  ftype <- typeof(fun)
  if (ftype == "closure") {

       #if there is no type information but the author put the type information on the function itself, use that.
     .typeInfo = attr(fun, "llvmTypes")
     if( (missing(returnType) || missing(types)) && !is.null(.typeInfo)) {
       if(missing(types))
          types = .typeInfo$parms
       if(missing(returnType))
          returnType = .typeInfo$returnType
     }
     

    args <- formals(fun) # for checking against types; TODO
    fbody <- body(fun)

    if(length(args)  > length(types))
      stop("need to specify the types for all of the arguments")

    if(length(names(types)) == 0)
      names(types) = names(args)
    
    # Find the name of the function if not provided
    if (is.null(name))
      name <- deparse(substitute(fun))

    rVecTypes = sapply(types, isRVectorType)
    if(any(rVecTypes)) {
       lengthVars = sprintf("_%s_length", names(types)[rVecTypes])
       types[lengthVars] = replicate(length(lengthVars), Int32Type)
    } else
       lengthVars = character()

     
    # Grab types, including return. Set up Function, block, and params.
    argTypes <- types
    llvm.fun <- Function(name, returnType, argTypes, mod)


    if(any(.globals$functions %in% names(.builtInRoutines))) {
       i = match(.globals$functions, names(.builtInRoutines), 0)
       .routineInfo = .builtInRoutines[ i ]
       .globals$functions = .globals$functions[i == 0]
    }

    if(name %in% .globals$functions && !(name %in% names(.functionInfo)))
       .functionInfo[[name]] = list(returnType = returnType, params = types)


    if(length(.routineInfo))
        processExternalRoutines(mod, .funcs = .routineInfo)

    if(length(.globals$functions)) 
       compileCalledFuncs(.globals, mod, .functionInfo)


    if(length(.globals$variables))
       compileGlobalVariables(.globals$variables, mod, env, ir)
    
    block <- Block(llvm.fun, "entry")
    params <- getParameters(llvm.fun)  # TODO need to load these into nenv
    ir <- IRBuilder(block)

    nenv = makeCompileEnv()

    nenv$.Rfun = fun
    nenv$.fun = llvm.fun
    nenv$.params = params
    nenv$.types = types
    nenv$.module = mod
    nenv$.compilerHandlers = .compilerHandlers
    nenv$.returnType = returnType
    nenv$.entryBlock = block
     
    nenv$.builtInRoutines = .builtInRoutines
    nenv$.functionInfo = .functionInfo
    nenv$.Constants = .constants
     
    compileExpressions(fbody, nenv, ir, llvm.fun, name)

    ## This may ungracefully cause R to exit, but it's still
    ## preferably to the crash Optimize() on an unverified module
    ## creates.
    if(optimize && verifyModule(mod))
       Optimize(mod)
     
    if(asFunction) {
       makeFunction(fun, llvm.fun, .vectorize = .vectorize, .execEngine = .execEngine, .lengthVars = lengthVars)
    } else if (asList)
      return(list(mod=mod, fun=llvm.fun, env = nenv))
    else
       llvm.fun
  } else if(!isIntrinsic(name))
     stop("compileFunction can only handle closures. Failing on ", name)
}

Rf_routines = c("length")
mapRoutineName =
function(name)
{
  w = name %in% Rf_routines
  name[w] = sprintf("Rf_%s", name[w])
  name
}

makeCompileEnv =
function()
{
   nenv <- new.env( parent = emptyenv())
   nenv$.continueBlock = list()
   nenv$.nextBlock = list()   
   nenv
}


isRVectorType =
  # This is transient and intended to identify if the
  # type corresponds to an R vector, rather than an R scalar.
  # For now this is an arrayType(, 0), i.e. with zero elements
function(type)
{
  is(type, "ArrayType") && getNumElements(type) == 0
}


processExternalRoutines =
function(mod, ..., .funcs = list(...), .lookup = TRUE)
{
  names(.funcs) = mapRoutineName(names(.funcs))
  
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
   # list(name = "bob", returnType = .., params = ...)
  if("returnType" %in% names(def)) {
     ret = def$returnType
     parms = def$params
  } else {
     ret = def[[1]]
     parms = def[-1]
  }
  fun = Function(name, ret, parms, module = mod)
  setLinkage(fun, ExternalLinkage)
  fun
}


getConstants =
function(..., .defaults = ConstantInfo)
{
  vals = list(...)
  .defaults[names(vals)] = vals
  .defaults
}

getBuiltInRoutines =
  #
  # See FunctionTypeInfo also 
  #
function()
{
        # These should be understood to be vectorized also.  
  list(exp = list(DoubleType, DoubleType),
       pow = list(DoubleType, DoubleType, DoubleType),
       sqrt = list(DoubleType, DoubleType),
       length = list(Int32Type, Rllvm:::getSEXPType("REAL")),
       nrow = list(Int32Type, c("matrix", "data.frame")),
       ncol = list(Int32Type, c("matrix", "data.frame")),
       dim = list(quote(matrix(Int32Type, 2)), c("matrix", "data.frame"))       
      )  
}

# Should this just be names of CompilerHandlers
ExcludeCompileFuncs = c("{", "sqrt", "return", MathOps,
                        LogicOps, "||", "&&", # add more here &, |
                        ":", "=", "<-", "[<-", '[', "for", "if", "while",
                        "repeat", "(", "!", "^")  # for now


compileCalledFuncs =
  #
  #  The .functionInfo
  #
function(globalInfo, mod, .functionInfo = list())
{
  funs = setdiff(globalInfo$functions, ExcludeCompileFuncs)

     # Skip the ones we already have in the module.
     # Possibly have different types!
  funs = funs[!(funs %in% names(getModuleFunctions(mod))) ]
  funs = funs[!(sapply(names(funs), isIntrinsic))]  
  
  funs = structure(lapply(funs, get), names = funs)


  lapply(names(funs),
           function(id) {
             if(id %in% names(.functionInfo)) {
               types = .functionInfo[[id]]
               compileFunction(funs[[id]],
                               types$returnType,
                               types = types$params,
                               mod = mod, name = id
                               )
             } else
               compileFunction(funs[[id]], mod = mod, name = id)
           })
}



makeFunction =
function(fun, compiledFun, .vectorize = character(),  .execEngine = NULL, .lengthVars = character())
{
  e = new.env()
  e$.fun = compiledFun
  if(is.null(.execEngine))
    .execEngine = ExecutionEngine(as(compiledFun, "Module"))
  
  args = c(as.name('.fun'), lapply(names(formals(fun)), as.name))
  k = call('run')
  k[2:(length(args) + 1)] = args

  if(length(.lengthVars))
    k[.lengthVars] = lapply(gsub("_(.*)_length", "\\1", .lengthVars),
                             function(id)
                               substitute(length(x), list(x = as.name(id))))
  
  k[[".ee"]] = as.name('.ee')
  
  formals(fun)$.ee = .execEngine
  body(fun) = k
  environment(fun) = e
  fun
}


isMutableRObject =
function(var)
{
   where = sapply(var, function(x) find(x)[1])
   if(any(is.na(where)))
     stop("Cannot find variables ", paste(var[is.na(where)], collapse = ", "))

   var %in% ".GlobalEnv"
}

compileGlobalVariables =
function(varNames, mod, env, ir,
          mutable = sapply(varNames, isMutableRObject))
{
   ctx = getGlobalContext()
   sapply(varNames[!mutable],
           function(var) {
             val = createConstant(ir, get(var), context = ctx)   
             createGlobalVariable(var, val, mod, constant = TRUE)
           })

#
   #XX create variables for the mutable ones.
}
