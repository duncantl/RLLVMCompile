fib =
  # Shouldn't need the L's on the interger constants
function(n)
{
  if(n == 0L || n == 1L)
     n
  else
     fib(n - 1L) + fib(n - 2L)
} 

if(FALSE) {
 library(RLLVMCompile)
 
 fc = compileFunction(fib, types = list( n = Int32Type), returnType = Int32Type)

   # check results
 sappy(0:11, function(n) run(fc, n))

   # compare times
 ti = system.time(fib(50))
 tc = system.time(run(fb, 50))

 library(compiler)
 fibc = cmpfun(fib)

 tt.raw = system.time(sapply(0:32, fib))
 tt.comp = system.time(sapply(0:32, fibc))

 fc = compileFunction(fib, types = Int32Type, returnType = Int32Type, optimize = FALSE)
   # get the module from the function.
 mod = getModule(fc)
 ee = ExecutionEngine(mod)
 Optimize(mod)
 
 tt.llvm = system.time(sapply(0:32, function(n) run(fc, n, .ee = ee)))
   # Not getting the speed that clang does which spits out very optimized IR code.
   # Is this because we need to add more optimization passes or because we are generating
   # less efficient IR that LLVM can't optimize later.

mod = parseIR('
define i32 @fib1(i32 %n) nounwind ssp {
      %1 = alloca i32, align 4
      %2 = alloca i32, align 4
      store i32 %n, i32* %2, align 4
      %3 = load i32* %2, align 4
      %4 = icmp eq i32 %3, 0
      br i1 %4, label %8, label %5

    ; <label>:5                                       ; preds = %0
      %6 = load i32* %2, align 4
      %7 = icmp eq i32 %6, 1
      br i1 %7, label %8, label %10

    ; <label>:8                                       ; preds = %5, %0
      %9 = load i32* %2, align 4
      store i32 %9, i32* %1
      br label %18

    ; <label>:10                                      ; preds = %5
      %11 = load i32* %2, align 4
      %12 = sub nsw i32 %11, 1
      %13 = call i32 @fib1(i32 %12)
      %14 = load i32* %2, align 4
      %15 = sub nsw i32 %14, 2
      %16 = call i32 @fib1(i32 %15)
      %17 = add nsw i32 %13, %16
      store i32 %17, i32* %1
      br label %18

    ; <label>:18                                      ; preds = %10, %8
      %19 = load i32* %1
      ret i32 %19
  }')
}
