source('load.R')

# Run existing tests
invisible(sapply(list.files("tests", pattern = ".R$", full.names = TRUE), source))

if (FALSE) {
  # some code to build empty basic functions
  mod = Module("sandbox")
  fun = Function("sandbox_not", retType=Int1Type, list(x=Int32Type), module=mod)
  block = Block(fun, "entry")
  ir = IRBuilder(block)
  params = getParameters(fun)

  ir$createReturn()
  
  ir$createReturn(x)

  showModule(mod)
}

