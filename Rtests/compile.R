## Test functions ##
add <- function(x, y) {
  xy.sum <- x + y # local var
  return(xy.sum)
}
## mod <- Module('testAdd')
## fun <- Function('Add', DoubleType, c(x=DoubleType, y=DoubleType), mod)
## block <- Block(fun)
## ir <- IRBuilder(block)
## params <- getParameters(fun)
## xy.sum <- createLocalVariable(ir, DoubleType, 'xysum')
## createStore(ir, xy.sum, 
## tmp = binOp(ir, Add, params$x, params$y)
## createReturn(ir, tmp)
## verifyModule(mod)

dumbAssign <- function() {
  x <- 3L
  return(x)
}
 
