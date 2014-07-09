library(RLLVMCompile)
EMPTY = 0L
RED = 1L
BLUE = 2L

moveCars = 
function(nc, rows, cols, dir = RED)
{
  nextCol = 0L
  ctr = 0L
  for(k in 1:length(rows)) {
      i = rows[k]

      if(dir == RED) {
	 nextCol = if(i <= nc ) i + 1L else 1L
       } else {
	 nextCol = if(i <= nc ) i + 1L else 1L
       }  

printf("HERE\n")

       if(nextCol == EMPTY) {
           ctr = nextCol
       }
  }
}


m = Module()
moveCars_f = compileFunction(moveCars, VoidType, 
                             list(Int32Type, INTSXPType, INTSXPType, Int32Type),
                              .constants = list(EMPTY = EMPTY, RED = RED, BLUE = BLUE), module = m)

