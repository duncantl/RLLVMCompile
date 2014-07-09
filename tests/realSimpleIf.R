library(RLLVMCompile)

f =
function(x)
{
   ans = 0L
   if(x < 2L)
       ans = 3L
# If there is no return expression, we get a failure in the compiler.   
#   ans
}

m = Module()
tryCatch(fc <- compileFunction(f, Int32Type, list(Int32Type), m),
          CompileError = function(e) {
              cat(class(e)[1], ":", e$message, "\n")
          })

m = Module()
tryCatch(fc <- compileFunction(f, VoidType, list(Int32Type), m),
          CompileError = function(e) {
              cat(class(e)[1], ":", e$message, "\n")
          })

