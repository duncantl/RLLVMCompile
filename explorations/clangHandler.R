#
# The idea here is to show how to a) write an R routine that will act as a
# function pointer (see functionPointer.R), b) access a C-level struct.
# 
#
#

library(RLLVMCompile)

h = function(cur, parent, data)
{
   ctr = ctr + 1L
   kinds[ctr] = cur$kind
   CXChildVisit_Recurse
}

mod = Module()
createGlobalVariable("ctr", mod, Int32Type, createIntegerConstant(0L))
createGlobalVariable("kind", mod, Int32Type, createIntegerConstant(0L))
createGlobalVariable("kinds", mod, arrayType(Int32Type, 1000000))

cursorType = structType(list(kind = Int32Type, xdata = Int32Type, data = arrayType(Int8Type, 3L)), "CXCursor")

library(RCIndex) # here because we need to find CXChildVisit_Recurse
fc = compileFunction(h, Int32Type, list( cursorType, cursorType, pointerType(Int8Type)), module = mod,
                       structInfo = list(CXCursor = cursorType))


ee = ExecutionEngine(mod)
fp = structure(getPointerToFunction(fc, ee)@ref, class = "NativeSymbol")

tu = createTU("testAlloc.c", includes = sprintf("%s/%s", R.home(), c("include", "../src/include")))
visitTU(tu, fp)
num = mod[["ctr", ee = ee]]  # value
kinds = mod[["kinds", ee = ee ]][1:num]
table(kinds)

ctr = 0L; visitTU(tu, function(...) { ctr <<- ctr + 1L; 2L})

rkinds = integer(ctr)
rctr = 1L
f = function(cur, parent)
{
   rkinds[rctr] <<- cur$kind
   rctr <<- rctr + 1L
   CXChildVisit_Recurse
}
visitTU(tu, f)

all(table(kinds) == table(rkinds))
identical(kinds, rkinds)



