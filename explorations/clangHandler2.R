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
   str = clang_CXCursor_getName(cur)
#   cxstr = clang_getCursorSpelling(cur)
#   val = clang_getCString(cxstr)
#   names[ctr] = 
   CXChildVisit_Recurse
}

mod = Module()
createGlobalVariable("ctr", mod, Int32Type, createIntegerConstant(0L))
createGlobalVariable("names", mod, arrayType(StringType, 10000))

CXCursorType = structType(list(kind = Int32Type, xdata = Int32Type, data = arrayType(Int8Type, 3L)), "CXCursor")
CXStringType = structType(list(data = pointerType(Int8Type), private_flags = Int32Type), "CXString")

Function("clang_getCursorSpelling", CXStringType, list(CXCursorType), mod)
Function("clang_getCString", StringType, list(CXStringType), mod)

# My own for testing
Function("clang_CXCursor_getName", StringType, list(CXCursorType), mod)

library(RCIndex) # here because we need to find CXChildVisit_Recurse
llvmAddSymbol("clang_getCString", "clang_getCursorSpelling", "clang_CXCursor_getName")

fc = compileFunction(h, Int32Type, list( CXCursorType, CXCursorType, pointerType(Int8Type)), module = mod,
                       structInfo = list(CXCursor = CXCursorType, CXString = CXStringType))

if(FALSE) {
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
}


