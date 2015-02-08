t2 <- function(x) {
    y <- 4
    x <- y
    return(x + 1)
}

ff = compileFunction(t2, DoubleType, DoubleType)
.llvm(ff, 3)
