## Builtin Types to overload
OPS <- list('for' = compileForLoop,
            'if' = ifHandler,
            '<-' = assignHandler,
            '=' = assignHandler,
            'while' = whileHandler,
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
#            '{' = function(call, env, ir) return(args), # TODO this doesn't work.
            '<' = logicOpHandler,
            '[' = subsetHandler,
            '[<-' = subsetAssignHandler,
            '(' = function(call, env, ir) compile(getArgs(call, env, ir)[[1]], env, ir),
            "break" = breakHandler,
            "next" = nextHandler,
            'repeat' = repeatHandler,
            'call' = callHandler
            )

LogicOps = c("<", ">", "<=", ">=", "!=", "==")
OPS[LogicOps] = replicate(length(LogicOps), logicOpHandler, simplify = FALSE)
OPS[MathOps] = replicate(length(MathOps), mathHandler, simplify = FALSE)
