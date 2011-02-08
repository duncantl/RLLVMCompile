invisible({library(Rllvm); invisible(sapply(list.files("R", pattern = ".R$", full.names = TRUE), source)); InitializeNativeTarget() ; sapply(sprintf("examples/%s", c("total.R", "if.R", "loop1.R", "mult.R", "break.R", "types.R", "repeat.R", "loopBreak.R", "callToOther.R")), source)})
 

a = compileFunction(fi.return, Int32Type)

a = compileFunction(loop.break, Int32Type)

#a = compileFunction(loop.next, Int32Type)

if(FALSE) { # now in compileFunction
  mod = Module("loopContinue")
  printi = Function("printi", VoidType, list(Int32Type), module = mod)
  setLinkage(printi, ExternalLinkage)
  llvmAddSymbol(printi = getNativeSymbolInfo("printi")$address)
  a = compileFunction(loop.next, Int32Type, mod = mod)
} else {

  a = compileFunction(loop.next, Int32Type, printi = list(VoidType, Int32Type))
}
run(a)


##############

a = compileFunction(foo, DoubleType, list(DoubleType, DoubleType))
b = compileFunction(bar, DoubleType, list(DoubleType, DoubleType), mod = as(a, "Module"))

run(a, 4, 5)
run(b, 4, 5)

##
# Now we'll attach type information to the functions and let compileFunction
# figure things out.
attr(foo, "llvmTypes") = list(returnType = DoubleType, parms = c(DoubleType, DoubleType))
attr(bar, "llvmTypes") = list(returnType = DoubleType, parms = c(DoubleType, DoubleType))
attr(foobar, "llvmTypes") = list(returnType = DoubleType, parms = c(DoubleType, DoubleType))

fb = compileFunction(foobar, sqrt = list(DoubleType, DoubleType))

run(fb, 4, 5) # 3

b = compileFunction(bar, sqrt = list(DoubleType, DoubleType))
run(b, 4, 5)  # 19





