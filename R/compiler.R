
compiler =
function(.compilerHandlers = getCompilerHandlers(), NAs = FALSE,
         .builtInRoutines = getBuiltInRoutines(),
         .functionInfo = list(),
         structInfo = list(),
         .zeroBased = logical(),
         .integerLiterals = TRUE,
         .useFloat = FALSE,
         .debug = TRUE, .assert = TRUE,
         .addSymbolMetaData = TRUE,
         .CallableRFunctions = list(), 
         ...,  compiler = makeCompileEnv())
{
    addToCompiler(compiler,
                    .compilerHandlers = .compilerHandlers,
                    .builtInRoutines = .builtInRoutines,
                    .functionInfo = .functionInfo,
                    .CallableRFunctions = .CallableRFunctions
                  )

    compiler$.NAs = NAs
    compiler$.structInfo = structInfo
    compiler$.loopDepth = 0L
    compiler$.zeroBased = .zeroBased
    compiler$.integerLiterals = .integerLiterals
    compiler$.useFloat = .useFloat
    compiler$.debug = .debug
    compiler$.assertFunctions = .assert
    compiler$.addSymbolMetaData = .addSymbolMetaData

    compiler$.SetCallFuns = list()
    compiler$.loopStack = character()

    compiler
}

addToCompiler =
function(compiler, ..., merge = TRUE)
{
  args = list(...)
  vars = ls(compiler, all = TRUE)
  mapply(function(id, val) {
           if(length(val)) {
               if(merge && id %in% vars) {
                   old = get(id, compiler)
                   old[names(val)] = val
                   val = old
               }
               assign(id, val, compiler)
           } else if(!merge)
               assign(id, val, compiler)
         }, names(args), args)
  
  compiler
}
