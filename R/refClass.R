BasicCompiler <-
 setRefClass("BasicCompiler",
            fields = list(
                 .compilerHandlers = "list",
                 .builtInRoutines = "list",
                 .Constants = "list",
                
                 .CallableRFunctions = 'list',
                 .functionInfo = 'list',
                 .structInfo = 'list',

                 .NAs = 'logical',
                 .useFloat = 'logical',
                 .zeroBased = 'logical',

                 .localVarTypes = 'list',
                 .loopDepth = 'integer',
                 .dimensionedTypes = 'list',

                 .module = "Module",                
                 .ExecEngine = "ExecutionEngine",
                 .SetCallFuns = "list",
                 .entryBlock = 'BasicBlock',
                 .returnType = 'Type',
                 .type = 'list',
                 .params = 'list',
                 .fun = 'Function',
                 .Rfun = 'ANY',

                 .continueBlock = 'BasicBlock',
                 .nextBlock = 'BasicBlock',
                 .funCalls = 'list',
                 .ir = "IRBuilder"
                ),
            methods = list(
               compile = function(obj, ...) {
                   RLLVMCompile::compile(obj, .self, .self$ir, ...)
               },
               declFunction = function(name)
                               declareFunction(.self$.builtInRoutines[[name]], name, .self$.module),
                addCallInfo = function(name, retType = NULL, types = NULL) {
                                  i = length(.self$.funCalls)
                                  .self$.funCalls[[i + 1L]] <<- list(name, returnType = retType, params = types)
                                  names(.self$.funCalls)[i + 1L] <<- name
                                  TRUE
                              }
                
             ))
