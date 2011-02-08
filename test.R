library(Rllvm); sapply(list.files("R", pattern = ".R$", full.names = TRUE), source); InitializeNativeTarget() ; source("R/guessType.R"); source("examples/total.R"); source("examples/if.R"); source("examples/loop1.R") ; source("examples/mult.R")

a = compileFunction(fi.return, Int32Type)

