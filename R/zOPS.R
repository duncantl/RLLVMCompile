## Builtin Types to overload
OPS <- list('for' = compileForLoop,
            'if' = ifHandler,
            '<-' = assignHandler,
            '=' = assignHandler,
            'while' = whileHandler,
            'return'=function(args, env, ir) {
              if (is.null(findVar('ReturnType', env)))
                stop("returnType must be in environment.")
              rt <- get('returnType', envir=env)
              # TODO check types -- how?
              checkArgs(args, list('ANY'), 'return')
              # cat("createReturn for '", args[[1]], "'\n", sep='')
              # cat("object:\n"); print(findVar(args[[1]], env)[[1]]); cat("\n")
              if(is.name(args[[1]]))
                 val = createLoad(ir, findVar(args[[1]], env)[[1]])
              else
                 val = args[[1]]
              
              createReturn(ir, val)
            },
            '{' = function(args, env, ir) return(args), # TODO this doesn't work.
            '<' = logicOpHandler,
            '[' = subsetHandler,
            '[<-' = subsetAssignHandler,
            '(' = function(args, env, ir) compile(args[[1]], env, ir),
            "break" = breakHandler,
            "next" = nextHandler,
            'repeat' = repeatHandler,
            'call' = callHandler
            )

LogicOps = c("<", ">", "<=", ">=", "!=", "==")
OPS[LogicOps] = replicate(length(LogicOps), logicOpHandler, simplify = FALSE)
OPS[MathOps] = replicate(length(MathOps), mathHandler, simplify = FALSE)
