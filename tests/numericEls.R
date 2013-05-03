library(RLLVMCompile)

f = function(x) {
  x[1] = pi
  x
}


f = function(x) {
  for(i in 1:length(x))
    x[i] = pi
  x
}

fc = compileFunction(f, REALSXPType, REALSXPType)
.llvm(fc, numeric(3))

f = function(x, y) {
  for(i in 1:length(x))
    x[i] = y[i]
  x
}

f = function(x, y) {
  for(i in 1:length(x))
    x[i] = 2 * y[i]
  x
}

fc = compileFunction(f, REALSXPType, list(REALSXPType, REALSXPType))
.llvm(fc, numeric(3), c(1, 2, 3))

