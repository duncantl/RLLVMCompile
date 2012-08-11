## Builtin Types to overload

`compile.(` = parenHandler =
function(call, env, ir, ...)
     compile(getArgs(call, env, ir)[[1]], env, ir)

returnHandler =
function(call, env, ir, ...) {
      args = as.list(call[-1])

      if (is.null(findVar('.returnType', env)))
        stop(".returnType must be in environment.")
      rt <- env$.returnType
            # TODO check types -- how?
      checkArgs(args, list('ANY'), 'return')


      argType = NULL
      if(is.name(args[[1]])) {
        val = getVariable(args[[1]], env, ir, load=TRUE, ...)

        argType = getType(args[[1]], env)
        if (is(argType, "Type"))
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

      if (!identical(argType, env$.returnType)) {
        message("Coercing type of return!")
        # We need to coerce types

        val = createCast(ir, env$.returnType, argType, val)                
      }
      ir$createReturn(val)
    }          

CompilerHandlers <-
       list(
            '!'=notHandler,  # Do we add this to LogicOps, or keep
                             # separate because it's unitary?
            'return'= returnHandler,
            '[' = subsetHandler,
            '[<-' = subsetAssignHandler,
            "break" = breakHandler,
            "next" = nextHandler,
            'repeat' = repeatHandler,
            'call' = callHandler
           )


CompilerHandlers[LogicOps] = replicate(length(LogicOps), logicOpHandler, simplify = FALSE)
CompilerHandlers[MathOps] = replicate(length(MathOps), mathHandler, simplify = FALSE)

