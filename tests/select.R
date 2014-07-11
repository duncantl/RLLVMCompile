library(RLLVMCompile)

f =
function(a)
{
  b = if(a < 10L) 2L else 4L
}

#debug(RLLVMCompile:::pushNextBlock)
m = Module()
fc = compileFunction(f, Int32Type, Int32Type, .fixIfAssign = FALSE, module = m, optimize = TRUE)
stopifnot(.llvm(fc, 4) == 2L)
stopifnot(.llvm(fc, 20) == 4L)


g =
function(x)
{
  n = Rf_length(x)
  total = 0L
  for(i in 1:n) {
     tmp = if(x[i] <= 5L) 1L else 0L
     total  = total + tmp
  }
  total
}

m = Module()
gc = compileFunction(g, Int32Type, INTSXPType, .fixIfAssign = FALSE, module = m, optimize = FALSE)
stopifnot(.llvm(gc, 1:10) == 5L)
#stopifnot(.llvm(gc, 20) == 4L)



o =
function(x)
{
  n = Rf_length(x)
  total = 0L
  for(i in 1:n) 
     total = total + if(x[i] <= 5L) 1L else 0L
  
  total
}
m = Module()
#debug(RLLVMCompile:::mathHandler)
oc = compileFunction(o, Int32Type, INTSXPType, .fixIfAssign = FALSE, module = m, optimize = FALSE)
stopifnot(.llvm(oc, 1:10) == 5L)


h =
function(x)
{
  n = Rf_length(x)
  total = 0L
  for(i in 1:n) {
     total  = (if(x[i] <= 5L) 1L else 0L) + total
  }
  total
}
m = Module()
hc = compileFunction(h, Int32Type, INTSXPType, .fixIfAssign = FALSE, module = m, optimize = FALSE)
stopifnot(.llvm(hc, 1:10) == 5L)


