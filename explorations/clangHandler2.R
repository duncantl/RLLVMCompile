#
# The idea here is to show how to a) write an R routine that will act as a
# function pointer (see functionPointer.R), b) access a C-level struct.
# 
# See clangHandler.R also and structByVal.R
#

library(RLLVMCompile)

h = function(cur, parent, data)
{
   ctr = ctr + 1L
   str = clang_CXCursor_getName(cur)
   Rprintf("%s\n", str)
#   cxstr = clang_getCursorSpelling(cur)
#   str = clang_getCString(cxstr)
   names[ctr] = strdup(str)
   CXChildVisit_Recurse
}

mod = Module()
createGlobalVariable("ctr", mod, Int32Type, createIntegerConstant(0L))
createGlobalVariable("names", mod, arrayType(StringType, 10000))

CXCursorType = structType(list(kind = Int32Type, xdata = Int32Type, data = arrayType(pointerType(Int8Type), 3L)), "CXCursor")

#CXStringType = structType(list(data = pointerType(Int8Type), private_flags = Int32Type), "CXString") # Should be unsigned.

#Function("clang_getCursorSpelling", CXStringType, list(CXCursorType), mod)
#Function("clang_getCString", StringType, list(CXStringType), mod)

# My own routine in RCIndex. (for simpler testing)
gn = Function("clang_CXCursor_getName", StringType, list(CXCursorType), module = mod)
Function("strdup", StringType, list(StringType), module = mod)
#Rprintf = Function("Rprintf", StringType, StringType, module = mod)

library(RCIndex) # here because we need to find CXChildVisit_Recurse
llvmAddSymbol("Rprintf", "clang_CXCursor_getName", "strdup")

fc = compileFunction(h, Int32Type, list( CXCursorType, CXCursorType, pointerType(Int8Type)), module = mod,
                       structInfo = list(CXCursor = CXCursorType)
#                       .builtInRoutines = getBuiltInRoutines(printf = Function("printf", Int32Type, list(StringType), varArgs = TRUE, module = mod))
                    )

if(TRUE) {
ee = ExecutionEngine(mod)
fp = structure(getPointerToFunction(fc, ee)@ref, class = "NativeSymbol")

tu = createTU("../tests/testAlloc.c", includes = sprintf("%s/%s", R.home(), c("include", "../src/include")))
visitTU(tu, fp)
num = mod[["ctr", ee = ee]]
ids = mod[["names", ee = ee]] # all 10,000 of the entries
sort(table(ids[1:num]))
}


