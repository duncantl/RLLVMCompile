## Builtin Types to overload

`compile.(` = parenHandler =
function(call, env, ir, ...)
     compile(getArgs(call, env, ir)[[1]], env, ir)

CompilerHandlers <-
       list(
            'return'= function(call, env, ir, ...) {
              args = getArgs(call, env, ir)
              if (is.null(findVar('.returnType', env)))
                stop(".returnType must be in environment.")
              rt <- env$.returnType
                    # TODO check types -- how?
              checkArgs(args, list('ANY'), 'return')

              if(is.name(args[[1]]))
                 val = createLoad(ir, findVar(args[[1]], env)[[1]])
              else
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


LogicOps = c("<", ">", "<=", ">=", "!=", "==")
CompilerHandlers[LogicOps] = replicate(length(LogicOps), logicOpHandler, simplify = FALSE)
CompilerHandlers[MathOps] = replicate(length(MathOps), mathHandler, simplify = FALSE)

