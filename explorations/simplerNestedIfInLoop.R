library(RLLVMCompile)

#trace(Block, quote(if(id == "after.for (k in 1:length(rows)) {") recover()))
#debug(Block)

#trace(createBr, quote(if(getName(getInsertBlock(builder)) == "after.if (dir == RED) {") recover()))

EMPTY = 0L
RED = 1L
BLUE = 2L



moveCars = 
function(nc, rows, cols, dir = RED)
{
  nextCol = 0L
  ctr = 0L
  for(k in 1:length(rows)) {
printf("start %d\n", k)      
      i = rows[k]

      if(dir == RED) {
         printf("   in RED\n")
#         ctr = ctr + 1L
	 nextCol = if(i <= nc ) i + 1L else 1L
       } else {
         printf("   in BLUE\n")
	 nextCol = if(i <= nc ) i + 1L else 1L
       }  

  printf("   testing EMPTY: \n") # %d, nextCol)
       if(nextCol == 1L) {
            printf("      is EMPTY\n")
           ctr = ctr + 1L
       }
#printf("end\n")
  }
  ctr
}



m = Module()
fc = compileFunction(moveCars, Int32Type, 
                             list(Int32Type, INTSXPType, INTSXPType, Int32Type),
                              .constants = list(EMPTY = EMPTY, RED = RED, BLUE = BLUE), module = m)

showModule(m)
stopifnot(.llvm(fc, 5, 1:10, 1:10, RED) == 5L)
#.llvm(fc, 5, 1:10, 1:10, BLUE)
