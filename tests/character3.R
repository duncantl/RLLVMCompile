library(RLLVMCompile)
readTo =
function(n)
{
   a = "abc"
   b = "xyz"
   tmp = character()
   if(n > 1)
     tmp = a
   else
     tmp = b

   tmp
}
if(FALSE) {##XXXX
fun = compileFunction(readTo, StringType, list(Int32Type), optimize = FALSE)

showModule(fun)
.llvm(fun, 2)
.llvm(fun, 1)
}

