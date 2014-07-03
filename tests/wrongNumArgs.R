library(RLLVMCompile)

f =
function(x)
{
  Rf_PrintValue(x, 1)
}

tryCatch(compileFunction(f, VoidType, list(SEXPType)),
          UserError = function(e) {
              browser()
                         cat("Caught problem in compilation - ", class(e)[1], ": ", e$message, ".\n", sep = "")
                       })

