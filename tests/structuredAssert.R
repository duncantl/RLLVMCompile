library(RLLVMCompile)
f =
function(x, y)
{
  .debug(printf("x = %d, y = %d\n", x, y))
  .assert(x > y, class = c("MyError", "MonotonicError"))
  x + y
}
fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), .debug = TRUE, .assert = TRUE)

tryCatch(.llvm(fc, 3, 5), error = function(e) print(class(e)))

tryCatch(.llvm(fc, 3, 5), MyError = function(e) cat("MyError:", e$message, "\n"))

