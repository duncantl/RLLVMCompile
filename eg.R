libary(Rllvm)
InitializeNativeTarget()

ptrDouble = pointerType(DoubleType)

mod <- Module('cumsum')
source("Cumsum.R")
compileFunction(Cumsum, ptrDouble, list(ptrDouble))
