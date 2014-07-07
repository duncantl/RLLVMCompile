library(RLLVMCompile)
f =
function(x, y)
{
  if(x < y)
     stop("x < y")
  x + y
}
fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), .debug = TRUE, .assert = TRUE)

tryCatch(.llvm(fc, 3, 5), error = function(e) print(class(e)))


f =
function(x, y)
{
  if(x < y)
     stop("x < y", class = "foo::MyError")
  x + y
}
fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), .debug = TRUE, .assert = TRUE)

tryCatch(.llvm(fc, 3, 5), error = function(e) print(class(e)))

tryCatch(.llvm(fc, 3, 5), `foo::MyError` = function(e) cat("MyError:", e$message, "\n"))

