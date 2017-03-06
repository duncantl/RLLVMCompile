library(RLLVMCompile)

f = function(x) "abc"
g = function(x) sapply(x, f)


f = function(x) "abc"
g = function(x) sapply(x, f)


g = function(x) sapply(x, f, 1, 2, 3)

