library(RLLVMCompile)
f =
function(x, n)
{
    total = 0.
    for(i in 1:n) 
        total = total + x[i]

    total
}
m = Module()
fc = compileFunction(f, DoubleType, list(pointerType(DoubleType), Int32Type), module = m)


g =
function(x, n, ans)
{
printf("hi\n")    
    for(i in 1:n) 
        ans[i] = 2. * x[i]
}
m = Module()
gc = compileFunction(g, VoidType, list(pointerType(DoubleType), Int32Type, pointerType(DoubleType)), module = m)
setMetadata(m, "g:ans", list("argStyle", "write-only"))

a = as.numeric(1:10)
#debug(.llvm)
ans = .llvm(gc, a, length(a), numeric(length(a)), .all = TRUE)[[3]]
