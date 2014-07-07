library(RLLVMCompile)
f =
function(x) {
    if (x > 10)
         printf("here\n")
}

fc = compileFunction(f, VoidType, Int32Type)
