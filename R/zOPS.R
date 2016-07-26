## Builtin Types to overload

`compile.(` = parenHandler =
function(call, env, ir, ..., .targetType = NULL)
     compile(getArgs(call, env, ir)[[1]], env, ir)

returnHandler =
function(call, env, ir, ..., .targetType = NULL) 
{
#XXX  connect with insertReturn.call and avoid doing anything here.

     if(length(call) == 1) {
         if(!sameType(env$.returnType, VoidType))
             stop("empty return but the routine expects to return ", as(env$.returnType, "character"))
         
         return(createRetVoid(ir))
     }
         
    
     if(is.call(call[[2]]) && !is.null(getSApplyType(call[[2]], env))) {
         # so now we have a return(sapply(...)) and so it contains a call to return(r_ans)
         # and we don't need to do this.
        compile(call[[2]], env, ir)
        #call = quote(return(r_ans))
        return(NULL)
      }

      args = as.list(call[-1])

      if (is.null(findVar('.returnType', env)))
        stop(".returnType must be in environment.")
      rt <- env$.returnType
            # TODO check types -- how? What check do we want to run?
      checkArgs(args, list('ANY'), 'return')


      argType = NULL
      if(is.name(args[[1]])) {
         val = getVariable(args[[1]], env, ir, load=TRUE, ...)

         argType = getDataType(args[[1]], env)
         if(is(argType, "Type") && !is(argType, "SEXPType"))
            argType = argType@ref
        
      } else if (is.call(args[[1]])) {
         val = compile(args[[1]], env, ir)
        
        # TODO We need to handle Rllvm calls in return
        # statements more generally, but for now, this works
        # for createNot (which returns an object of class
        # BinaryOperator).
        if (is(val, "BinaryOperator"))
          argType = Int32Type

      } else if (is.numeric(args[[1]])) {
        if (is.integer(args[[1]]))
          val = createIntegerConstant(as.integer(args[[1]]))
        if (is.double(args[[1]]))
          val = createDoubleConstant(as.double(args[[1]]))
      } else
         val = args[[1]]

      # What's the current type of the arugment?

      if (is.null(argType)) {
        argType = Rllvm::getType(val)
        if (is(argType, "Type"))
          argType = argType@ref ## get externalptr
        else
          stop("Could not getType.")
      }

      if (!sameType(argType, env$.returnType)) {
        message("Coercing type of return!")
           # We need to coerce types
        val = createCast(env, ir, env$.returnType, argType, val)                
      }
      ir$createReturn(val)
    }




getCompilerHandlers <-
function(...)
{  
   CompilerHandlers =   list(
            '!' = notHandler,  # Do we add this to LogicOps, or keep
                               # separate because it's unitary?
            'return'= returnHandler,
            '[' = subsetHandler,
            '[[' = subsetDoubleHandler,       
            '[<-' = subsetAssignHandler,
            "break" = breakHandler,
            "next" = nextHandler,
            'repeat' = repeatHandler,
#            'call' = callHandler,
                  #XXX These are needed for the rewriteSApply() code which doesn't seem to dispatch. Any programmatically generated code?
             'for' = `compile.for`,
#            '<-' = `compile.=`,
            '$' = `compile.$`                        
           )


   CompilerHandlers[LogicOps] = replicate(length(LogicOps), logicOpHandler, simplify = FALSE)
   CompilerHandlers[MathOps] = replicate(length(MathOps), mathHandler, simplify = FALSE)

     # add any others user supplied.
   others = list(...)
   CompilerHandlers[names(others)] = others

   if(!("=" %in% names(CompilerHandlers)))
        CompilerHandlers[["="]] = CompilerHandlers[["<-"]]
 
  invisible(CompilerHandlers)
}
