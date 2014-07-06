library(RLLVMCompile)
f =
function(x, y)
{
  if(x < y)
     warning("x < y", class = "MyError")
  x + y
}
fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), .debug = TRUE, .assert = TRUE)

tryCatch(.llvm(fc, 3, 5), warning = function(e) cat("got a warning of class ", class(e)[1], "\n"), error = function(e) print(class(e)))
