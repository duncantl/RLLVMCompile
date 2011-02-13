source('load.R')

if (FALSE) {
  # some code to build empty basic functions
  mod = Module("sandbox")
  fun = Function("sandbox_not", retType=Int1Type, list(x=Int32Type), module=mod)
  block = Block(fun, "entry")
  ## b.pos = Block(fun, "value.pos")
  ## b.neg = Block(fun, "value.neg")
  ir = IRBuilder(block)
  params = getParameters(fun)

  x = ir$createTrunc(params$x, 1L)
  ir$createReturn(x)
  #ins = ir$binOp(Xor, x, ir$createIntegerConstant(1L, bitwidth=1L))

  ## pos = createICmp(ir, ICMP_SGT, params$x, createIntegerConstant(0L))
  ## createCondBr(ir, pos, b.pos, b.neg)

  ## setInsertPoint(ir, b.pos)
  ## x = ir$createTrunc(params$x, 1L)
  ## ir$createReturn(x)

  ## setInsertPoint(ir, b.neg)
  ## ir$binOp(Sub, 0L, params$x)
  ## x = ir$createTrunc(params$x, 1L)
  ## ir$createReturn(x)

  ## makeFunction(not.fun, fun)
  showModule(mod)
}


# Not
## a = compileFunction(not.fun, Int32Type, list(Int32Type), asList=TRUE)
## showModule(a$mod)

## run(a$fun, 1L)


## # Most basic test
## f1 = function(x, y) {
##   return(x + y)
## }

## a = compileFunction(f1, DoubleType, list(DoubleType, DoubleType), asList=TRUE)


## Branch test -- fails
if (FALSE) {
  fi.1 = function(x) {
    if(x > 0) {
      return(1)
    } else {
    return(0)
  }
  }
  
  b = compileFunction(fi.1, DoubleType, list(DoubleType), asList=TRUE, optimize=FALSE)
}


##
f = function(x) {
  c <- 3
  y <- x + c
  return(y)
}

b = compileFunction(f, DoubleType, list(DoubleType), asList=TRUE, optimize=FALSE)
