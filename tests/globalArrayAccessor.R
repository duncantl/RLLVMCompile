library(RLLVMCompile)

mod = Module()
mod[["kinds"]] = arrayType(Int32Type, 100)
# No longer needed. Set in createGlobalVariable
#     setInitializer(mod[["kinds"]], constantAggregateZero(ty))

f = function() {
      kinds[2L]
    }

g = function() {
        kinds[2L] = 10L
     }

if(TRUE) {
fc = compileFunction(f, Int32Type, module = mod)
gc = compileFunction(g, VoidType, module = mod)


ee = ExecutionEngine(mod)
.llvm(fc, .ee = ee)
a = mod[["kinds", ee = ee]]
a[2]
.llvm(gc, .ee = ee)
.llvm(fc, .ee = ee)
b = mod[["kinds", ee = ee]]
b[2]
b[2] == 10L
}

