library(codetools)
library(RLLVMCompile)

fun = compileFunction

fun =
function()
{
  ctr = 20L
  a = g(ctr)
  ans = foo(bob, x = ctr, structure(1:10, class = "Jane", names = letters[1:10]))
  ans
}


#############

w = makeCodeWalker(call = function(e, w) { print(e[[1]]) ; lapply(e[-1], walkCode, w)})
invisible(walkCode(body(fun), w))



h = function(e, w)
{
  if(is.call(e)) {
      cat("cur function", as.character(e[[1]]), "\n")
  } else if(is.name(e))
      cat("name:", as.character(e), "\n")
  lapply(e[-1], walkCode, w)
}
leaf = function(e, w) cat("leaf:", as.character(e), "\n")
w = makeCodeWalker(call = h, leaf = leaf)
invisible(walkCode(body(fun), w))


w = makeCodeWalker(call = function(e, w) { print(e[[1]]) ; lapply(e[-1], walkCode, w)})
invisible(walkCode(body(fun), w))


##########







e = function(type, v, e, w) {
  if(typeof(e[[1]]) %in% c("symbol", "character")) {
      browser()
  }
  walkCode(e[[1]], w)
}
collectUsage(compileFunction, formals(fun),  body(fun), enterGlobal = e)

w = makeUsageCollector(fun, call = e) # , enterLocal = function(...) cat("enterLocal\n"))

w = makeCodeWalker(call = e)
codetools:::collectUsageFun("compileFunction", formals(fun), fun, w)


#######
procCall = function(e, w) {
    cat("procCall\n")
    print(e[[1]])
              for (ee in as.list(e))
                if (! missing(ee))
                      walkCode(ee, w)
          }
# if(type == "call") { cat("call to", as.character(call[[1]]), "\n"); procCall}
w = makeCodeWalker(handler = function(v, w)  browser(),
                   call = function(v, w) browser())
# call  = function(e, w) print(class(e)))
invisible(walkCode(quote(foo(1, y, h(x))), w))
invisible(walkCode(body(fun), w))

################
w = makeCodeWalker( handler = function(v, w) { print(class(v)) ; function(...) {cat("done\n")} })
invisible(walkCode(fun, w))
# w = makeCodeWalker()
# 

# makeCodeWalker
# walkCode
