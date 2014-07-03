library(RLLVMCompile)

f =
function(x, y)
{
  .debug(printf("x = %d, y = %d\n", x, y))
  .assert(x > y)
  x + y
}

fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), .debug = FALSE, .assert = FALSE)
showModule(fc)
.llvm(fc, 3, 5)

fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), .debug = FALSE, .assert = TRUE)
showModule(fc)
.llvm(fc, 5, 3)
tryCatch(.llvm(fc, 3, 5),
          error = function(e) cat("Caught the error:", e$message, "\n"))

fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), .debug = TRUE, .assert = TRUE)
showModule(fc)
.llvm(fc, 5, 3)

f =
function(x, y)
{
  .debug(printf("x = %d, y = %d\n", x, y))
  assertthat(x > y)
  x + y
}
fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), .debug = TRUE, .assert = "assertthat")
.llvm(fc, 3, 5)


f =
function(x, y)
{
  .debug(printf("x = %d, y = %d\n", x, y))
  assertthat(x > y)
  x + y
}
fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), .debug = TRUE, .assert = "assertthat")
.llvm(fc, 3, 5)

f =
function(x, y)
{
  .debug(printf("x = %d, y = %d\n", x, y))
  assertthat(x > y, class = c("MyError", "MonotonicError"))
  x + y
}
fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), .debug = TRUE, .assert = "assertthat")
.llvm(fc, 3, 5)
