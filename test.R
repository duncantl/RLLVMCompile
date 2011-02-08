library(Rllvm); sapply(list.files("R", pattern = ".R$", full.names = TRUE), source); InitializeNativeTarget() ; sapply(sprintf("examples/%s", c("total.R", "if.R", "loop1.R", "mult.R", "break.R", "types.R", "repeat.R", "loopBreak.R")), source)

a = compileFunction(fi.return, Int32Type)

a = compileFunction(loop.break, Int32Type)

a = compileFunction(loop.next, Int32Type)

mod = Module("loopContinue")
printi = Function("printi", VoidType, list(Int32Type), module = mod)
setLinkage(printi, ExternalLinkage)
llvmAddSymbol(printi = getNativeSymbolInfo("printi")$address)
a = compileFunction(loop.next, Int32Type, mod = mod)

run(a)



