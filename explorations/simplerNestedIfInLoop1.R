library(RLLVMCompile)

#trace(Block, quote(if(id == "after.for (k in 1:length(rows)) {") recover()))
#debug(Block)

#trace(createBr, quote(if(getName(getInsertBlock(builder)) == "after.if (dir == RED) {") recover()))

EMPTY = 0L
RED = 1L
BLUE = 2L


moveCars = 
function(grid, nr, nc, rows, cols, dir = RED)
{
  ctr = 0L

  for(k in 1:length(rows)) {
      i = rows[k]
      j = cols[k]

      if(dir == RED) {
         nextRow = i
	 nextCol = if(i <= nc ) i + 1L else 1L
       } else {
         nextCol = j
	 nextRow = if(j <= nr ) j + 1L else 1L
       }  

printf("HERE\n")

       if(grid[nextRow, nextCol] == EMPTY) {
printf("Moving car\n")           
          ctr = ctr + 1L
          grid[nextRow, nextCol] = dir
	  grid[i, j] = EMPTY
          rows[k] = nextRow
          cols[k] = nextCol
       } else {
           printf("not moving car\n")
       }

#     printf("next\n") # if we add this, get an infinite loop. So jumping to the wrong place.
      
  }
  ctr
#  printf("num moved %d\n", ctr)
}



m = Module()
fc = compileFunction(moveCars, Int32Type, 
                             list(MatrixType(Int32Type), Int32Type, Int32Type, INTSXPType, INTSXPType, Int32Type),
                              .constants = list(EMPTY = EMPTY, RED = RED, BLUE = BLUE), module = m)

showModule(m)


g = matrix(0L, 3,5)
red.rows = as.integer(c(1, 2, 2))
red.cols = as.integer(c(1, 2, 4))
blue.rows = as.integer(c(2, 1, 1, 2))
blue.cols = as.integer(c(1, 3, 4, 5))
g[cbind(red.rows, red.cols)] = 1L
g[cbind(blue.rows, blue.cols)] = 2L

stopifnot(.llvm(fc, g, nrow(g), ncol(g), red.rows, red.cols, RED) == 2L)

