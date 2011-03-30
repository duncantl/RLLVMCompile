invisible({
  library(codetools)
  library(Rllvm)
  invisible(sapply(list.files("R", pattern = ".R$", full.names = TRUE), source))
  InitializeNativeTarget()
  sapply(sprintf("inst/examples/%s", c("total.R", "if.R", "loop1.R", "mult.R", "break.R", "types.R", "repeat.R", "loopBreak.R", "callToOther.R", "not.R")), source)
})
 
