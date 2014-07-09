library(RLLVMCompile)

if(TRUE) {
f =
function(x)
{
   ans = 0L
   if(x < 2L)
       ans = 3L
   ans
}

m = Module()
fc <- compileFunction(f, Int32Type, list(Int32Type), m, optimize = FALSE)
stopifnot(.llvm(fc, 1) == 3L)
stopifnot(.llvm(fc, 2) == 0L)


g =
function(x)
{
   ans = 0L
   if(x < 2L)
       ans = 3L
   else
       ans = 10
   ans
}
gc <- compileFunction(g, Int32Type, list(Int32Type), m, optimize = FALSE)
stopifnot(.llvm(gc, 1) == 3L)
stopifnot(.llvm(gc, 2) == 10L)
}


if(TRUE) {
cat("Working on h\n")    
h =
function(x)
{
   ans = 0L
   for(i in 1:10) {
       printf("%d\n", i)
       if(x < 2L)
           ans = 3L
       else
           ans = 10
   }
   ans
}
m = Module()
hc <- compileFunction(h, Int32Type, list(Int32Type), m, optimize = TRUE)
showModule(hc)
stopifnot(.llvm(hc, 1) == 3L)
stopifnot(.llvm(hc, 2) == 10L)
}

if(TRUE) {
k =
function(x)
{
   ans = 0L
   tmp = 0L
   for(i in 1:10) {
       printf("%d\n", i)
       if(i < 2L) {  # Here, we compare i with 2, not x with 2.
           ans = 3L
       } else
           ans = 10
   }
   ans
}
m = Module()
kc <- compileFunction(k, Int32Type, list(Int32Type), m, optimize = TRUE)
showModule(hc)
stopifnot(.llvm(kc, 1) == 10L)
stopifnot(.llvm(kc, 2) == 10L)
}
