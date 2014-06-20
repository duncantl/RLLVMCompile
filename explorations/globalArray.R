library(RLLVMCompile)

fun =
function() {
 ctr = ctr + 1L
 tmp = kinds[1L] 
 return(tmp)
}

mod = Module()
createGlobalVariable("kinds", mod, arrayType(Int32Type, 1000000))
createGlobalVariable("ctr", mod, Int32Type, createIntegerConstant(0L))

debug(createGEP)

fc = compileFunction(fun, Int32Type, module = mod)
