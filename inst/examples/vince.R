## mod <- Module('testDumbAssign')
## fun <- Function('dumbAssign', Int32PtrType, c(), mod)
## block <- Block(fun)
## ir <- IRBuilder(block)
## #e <- new.env()
## #a <- OPS$`<-`(getArgs(body(dumbAssign)[[2]]), e, ir)
## #createReturn(ir, get('x', envir=e))
## x <- createLocalVariable(ir, Int32Type, "x")
## createStore(ir, createLoad(ir, createConstant(ir, 3L)), x)
## createReturn(ir, x)
## verifyModule(mod)


##
if(FALSE) {
mod <- Module('testDumbAssign')
m <- compileFunction(dumbAssign, mod, types=c(returnType=DoubleType))
verifyModule(m$mod)  # compileFunction should take care of this.
run(m$fun)

Optimize(m$fun)
}
