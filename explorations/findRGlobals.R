library(codetools)
library(RLLVMCompile)

fun = compileFunction

e = function(type, v, e, w) print(v)
collectUsage(compileFunction, formals(fun),  body(fun), enterGlobal = e)

w = makeUsageCollector(fun, enterGlobal = e, enterLocal = function(...) cat("enterLocal\n"))
codetools:::collectUsageFun("compileFunction", formals(fun), fun, w)


################
w = makeCodeWalker( handler = function(v, w) { print(class(v)) ; function(...) {cat("done\n")} })
invisible(walkCode(fun, w))
# w = makeCodeWalker()
# 

# makeCodeWalker
# walkCode
