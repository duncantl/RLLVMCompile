library(RLLVMCompile)

f = function(x) {
       x[2]
   }

# This will seg fault since not subsettable. Catch. Fixed now.
# compileFunction(f, Int32Type, list(Int32Type))

compileFunction(f, Int32Type, list(pointerType(Int32Type)))

