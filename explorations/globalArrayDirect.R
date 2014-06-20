library(Rllvm)
InitializeNativeTarget()

mod = Module()

 # create a global variable that is an array of 1 million int elements.
kinds = createGlobalVariable("kinds", mod, arrayType(Int32Type, 1000000))

# This function retrieves the first element of the array.
foo = fun = Function("foo", Int32Type, module = mod)
entry = Block(fun, "entry")
ir = IRBuilder(entry)

ElementIndex = 789L

gep = ir$createGEP(kinds, list(createIntegerConstant(0L, getContext(mod)), createIntegerConstant(ElementIndex, getContext(mod))))
val = ir$createLoad(gep)
ir$createRet(val)

showModule(mod)

#########
# Set an element in the array. Same element as we get in the function foo above.

bar = Function("bar", VoidType, module = mod)
entry = Block(bar, "entry")
ir = IRBuilder(entry)

gep = ir$createGEP(kinds, list(createIntegerConstant(0L, getContext(mod)), createIntegerConstant(ElementIndex, getContext(mod))))
ir$createStore(createIntegerConstant(ElementIndex, getContext(mod)), gep)
ir$createRetVoid()

showModule(mod)

ee = ExecutionEngine(mod)
.llvm(bar, .ee = ee)
ans = .llvm(foo, .ee = ee)

stopifnot(ans == ElementIndex)

# See globalArray.R
# fc = compileFunction(fun, Int32Type, module = mod)
