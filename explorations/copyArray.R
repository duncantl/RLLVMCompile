library(RLLVMCompile)

copyDoubleArray =
function(src, n)
{
    ans = numeric(n)
    for(i in 1:n)
        ans[i] = src[i]
    ans
}

cp = compileFunction(copyDoubleArray, REALSXPType, list(DoublePtrType, Int32Type))
