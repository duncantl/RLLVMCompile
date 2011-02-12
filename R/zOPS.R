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

              if(is.name(args[[1]]))
                val = createLoad(ir, findVar(args[[1]], env)[[1]])
              else if (is.call(args[[1]]))
                val = compile(args[[1]], env, ir)
              else if (is.numeric(args[[1]])) {
                if (is.integer(args[[1]]))
                  val = createIntegerConstant(as.integer(args[[1]]))
                if (is.double(args[[1]])) ## This still doesn't handle
                                          ## special unary types, so
                                          ## return(-1L) doesn't work.
                  val = createDoubleConstant(as.double(args[[1]]))
              } else
                 val = args[[1]]
              
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

