library(RLLVMCompile)

fun =
function() {
 ctr = ctr + 1L
 tmp = kinds[1L] 
 return(tmp)
}

g = function()
     ctr

mod = Module()
kinds = createGlobalVariable("kinds", mod, arrayType(Int32Type, 1000000))
ctr = createGlobalVariable("ctr", mod, Int32Type, createIntegerConstant(0L))

# debug(createGEP)

fc = compileFunction(fun, Int32Type, module = mod)
gc = compileFunction(g, Int32Type, module = mod)

ee = ExecutionEngine(mod)
.llvm(fc, .ee = ee)

replicate(10, .llvm(fc, .ee = ee))
mod[["ctr", value = TRUE, ee = ee]]
