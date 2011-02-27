## Builtin Types to overload

`compile.(` = parenHandler =
function(call, env, ir, ...)
     compile(getArgs(call, env, ir)[[1]], env, ir)

CompilerHandlers <-
       list(
            '!'=notHandler,  # Do we add this to LogicOps, or keep
                             # separate because it's unitary?
            'return'= function(call, env, ir, ...) {
              args = as.list(call[-1])

              if (is.null(findVar('.returnType', env)))
                stop(".returnType must be in environment.")
              rt <- env$.returnType
                    # TODO check types -- how?
              checkArgs(args, list('ANY'), 'return')

              argType = NULL
              if(is.name(args[[1]])) {
                val = getVariable(args[[1]], env, ir, load=TRUE, ...)
                argType = getTypes(args[[1]], env)@ref
              } else if (is.call(args[[1]])) {
                val = compile(args[[1]], env, ir)
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
                ## toTypes = c(Int32Type=Int32Type, DoubleType=DoubleType)
                ## fromTypes = c(DoubleType=DoubleType, Int32Type=Int32Type)
                ## casters = c(CreateSIntToFPInst, CreateFPToSIInst)
                
                ## i <- which(sapply(fromTypes, function(x) identical(argType, x)))
                ## fun = casters[[i]]
                ## val = fun(ir, val, Int32Type)
              }
              print(val)
              ir$createReturn(val)
            },
            '[' = subsetHandler,
            '[<-' = subsetAssignHandler,
            "break" = breakHandler,
            "next" = nextHandler,
            'repeat' = repeatHandler,
            'call' = callHandler
           )


CompilerHandlers[LogicOps] = replicate(length(LogicOps), logicOpHandler, simplify = FALSE)
CompilerHandlers[MathOps] = replicate(length(MathOps), mathHandler, simplify = FALSE)

