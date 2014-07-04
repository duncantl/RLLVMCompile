library(RLLVMCompile)

f =
function(x, y = x + 1, b = 10*a, d = 3)
{
  a = 1
  w =  x * y + a * b
  w = w + d + 1
  w
}

library(CodeDepends)

sc = new("Script", as.list(body(f))[-1])
inputs = getInputs(sc)

hasDef = sapply(formals(f), hasDefault)


lapply(names(formals(f))[hasDef],
       function(var)
          sapply(inputs, function(x) var %in% x@inputs))

usesY = sapply(inputs, function(x) "y" %in% x@inputs)
which(usesY)


