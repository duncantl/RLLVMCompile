#
# The idea here is to show how to a) write an R routine that will act as a
# function pointer (see functionPointer.R), b) access a C-level struct.
# 
#
#

library(RLLVMCompile)

# do we need to add the inbounds to the getelementptr for cur kind
# Do we need the align 8 on the byval parameters
# Should we have %struct.CXCursor rather than CXCursor
# compare code we generate with clangStruct.ll
 
h = function(cur, parent, data)
{
      # put the kind of the current cursor at the end of the kinds array
      # so there will be an entry for each cursor we visit.
   ctr = ctr + 1L
   kinds[ctr] =  cur$kind
   CXChildVisit_Recurse
}

hh = function(cur, parent, data) {
   ctr = ctr + 1L
   tmp = cur$kind
   kinds[ctr] =  2 #
   CXChildVisit_Recurse    
}

mod = Module()
createGlobalVariable("ctr", mod, Int32Type, createIntegerConstant(0L))
createGlobalVariable("kind", mod, Int32Type, createIntegerConstant(0L))
createGlobalVariable("kinds", mod, arrayType(Int32Type, 1000000))

cursorType = structType(list(kind = Int32Type, xdata = Int32Type, data = arrayType(pointerType(Int8Type), 3L)), "CXCursor")

library(RCIndex) # here because we need to find CXChildVisit_Recurse
fc = compileFunction(h, Int32Type, list( cursorType, cursorType, pointerType(Int8Type)), module = mod,
                       structInfo = list(CXCursor = cursorType))

reset = function() ctr <- 0L
# Could add  kinds[] = 0L and have compileFunction recognize that as a call to memset()
creset = compileFunction(reset, VoidType, module = mod)

ee = ExecutionEngine(mod)
fp = structure(getPointerToFunction(fc, ee)@ref, class = "NativeSymbol")

tu = createTU("../tests/testAlloc.c", includes = sprintf("%s/%s", R.home(), c("include", "../src/include")))
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
stopifnot(identical(kinds, rkinds))

i = match(rkinds, CXCursorKind)
names(rkinds) = names(CXCursorKind)[i]

.llvm(creset, .ee = ee)

tm.ll = system.time(visitTU(tu, fp))
tm.rr = system.time(visitTU(tu, f))


