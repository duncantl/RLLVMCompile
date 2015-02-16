library(RLLVMCompile)

# See ../explorations/clangHandler{,2}.R


# This example relates to the RCIndex package.
# We provide a function that is called as visitTU()
# iterates over a tree. The function is called for each node.
# It is passed the current node, the parent node and arbitrary user-supplied data.
# Interestingly, the node and parent node are passed by value, not by address.
# The nodes are C-level structures. The struct has a kind field and additional
# opaque data that is specific to the kind.

# We will write a simple function that is called as we visit each node.
# The function  prints the type of the node which we get via the kind element of the
# C structure.
h = function(cur, parent, data)
{
   ctr = ctr + 1L
   kind = cur$kind
   printf("%d %d\n", ctr, kind)
   2L  # this means continue on to the child nodes.
}

#llvmAddSymbol("printf")
mod = Module()
mod[["ctr"]] = 1L
cursorType = structType(list(kind = Int32Type, xdata = Int32Type, data = arrayType(pointerType(Int8Type), 3L)), "CXCursor")

fc = compileFunction(h, Int32Type, list( cursorType, cursorType, pointerType(Int8Type)), module = mod,
                       structInfo = list(CXCursor = cursorType))

ee = ExecutionEngine(mod)
fp = structure(getPointerToFunction(fc, ee)@ref, class = "NativeSymbol")

library(RCIndex)
tu = createTU("testAlloc.c", includes = sprintf("%s/%s", R.home(), c("include", "../src/include")))
visitTU(tu, fp)

 # Need the ee = ee to get the updated value.
print(mod[["ctr", ee = ee]])

if(FALSE) {
printf = function(fmt, ...) cat(sprintf(fmt, ...))
ctr = 0L
visitTU(tu, h)
}




