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
     cat("createLocalVariable for '", var, "'\n", sep='')
     type = getType(val, env)
     assign(var, createLocalVariable(ir, type, var), envir=env) ## Todo fix type
     env$.types[[var]] = type
   }
   
   ref <- get(var, envir = env)

      # Now, create store. TODO: how does this work *without*
      # constants? Where is evaluation handled... probably not
      # here?
   cat("createStore for '", var, "'\n", sep='')
   cat("object:\n"); print(val); cat("\n")
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


MathOps = c("+", "-", "*", "/", "%/%")
mathHandler =
  #
  # This currently (Feb 7, 2pm) attempts to do a little bit
  # of figuring about the types of the two operands.
  # It just handles integer and numeric types for now.
  # 
  #
function(call, env, ir, ..., isSubsetIndex = FALSE)  
{
     # Need to hadle the case where we have a literal and we can convert it to the
     # target type.

  if(any( lit <- sapply(call[-1], is.numeric))) { # literals
     targetType = getTypes(as.list(call[-1])[!lit][[1]], env)
  } else {
  
     #??? We may want to compile first and then determine types and then coerce.
     # Compute the type of each, returning the LLVM type objects, e.g. DoubleType
    types = lapply(call[-1], getTypes, env)
     # Collapse these two types to the "common" type
    targetType = getMathOpType(types)
  }
  isIntType = identical(targetType, Int32Type)
  e = lapply(call[-1], function(x)
                       if(is(x, "numeric")) {
                          if(isIntType)
                             createIntegerConstant(as.integer(x))
                          else
                             createDoubleConstant(as.numeric(x))
                       } else if(is.name(x)) {
                            # Potentially have to cast based on the target type
                         getVariable(x, env, ir)
                       } else
                         compile(x, env, ir, ...))

    # XXX Have to deal with different types.
  if(isIntType)
     codes = c("+" = Add, "-" = Sub, "*" = Mul, "-" = SDiv, "%/%" = SRem)
  else
     codes = c("+" = FAdd, "-" = FSub, "*" = FMul, "-" = FDiv, "%/%" = FRem)

  

  op = codes[ as.character(call[[1]]) ]

  
  ins = ir$binOp(op, e[[1]], e[[2]])
  ins
}

getVariable =
function(sym, env, ir = NULL, load = TRUE, ...)
{

  sym = as.character(sym)
  var = if(exists(sym, env)) {
               # The local variables we create in the function
               # are alloc'ed and so are pointers. They need to be
               # loaded to use their values.
          tmp = get(sym, env)
          if(load && !is.null(ir))
             ir$createLoad(tmp)
          else
            tmp
        } else {
          env$.params[[sym]]
        }
}

logicOpHandler =
function(call, env, ir, ...)
{
    # need to handle the different ops
    # and the different types, casting
    # if necessary.
  op = as.character(call[[1]])

  types = lapply(call[-1], getTypes, env)
  targetType = getMathOpType(types)
  isIntType = identical(targetType, Int32Type)  

  a = compile(call[[2]], env, ir)   # getVariable(call[[2]], env, ir)
  b = compile(call[[3]], env, ir)   # getVariable(call[[3]], env, ir)

  if(isIntType)
     codes = c("==" = ICMP_EQ, "!=" = ICMP_NE, ">" = ICMP_SGT, "<" = ICMP_SLT, ">=" = ICMP_SGE, "<=" = ICMP_SLE)
  else
     codes = c("==" = FCMP_UEQ, "!=" = FCMP_UNE, ">" = FCMP_UGT, "<" = FCMP_ULT, ">=" = FCMP_UGE, "<=" = FCMP_ULE)    
  
  op = codes[ as.character(call[[1]]) ]
  if(is.na(op))
     stop("Unhandled logical operator")

  if(isIntType)
    ir$createICmp(op, a, b)    
  else
     ir$createFCmp(op, a, b)
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

       # For side effects of building mod
       # Have to be careful with one line functions with no {
    bdy = fbody

    compileExpressions(bdy, nenv, ir, llvm.fun, name)    # TODO class { should be handled
                                              # better.  Can just do it as a separate block.
#    return(list(mod=mod, fun=llvm.fun, env = nenv))
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
