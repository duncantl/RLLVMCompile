source("BML.R")
##########################################################################################
#
# The following functions are the ones we compile.
#
#

#
# This function assumes we are mutating g, rrows, rcols, brows and bcols in
# the calls to moveCars, i.e. we are passing the pointers to the collections of elements
# in each.
#
runBML = 
function(g, rrows, rcols, brows, bcols, niter)
{
   nc = Rf_ncols(g)
   nr = Rf_nrows(g)
   nred = Rf_length(rrows)
   nblue = Rf_length(brows)
  
   for(i in 1:niter) {
      moveCars(g, rrows, rcols, nred, nr, nc,  RED)
      moveCars(g, brows, bcols, nblue, nr, nc, BLUE)
   }
}

moveCars =
    #
    #  When we know we are compiling runBML() and moveCars() together
    #  and we are calling moveCars() from runBML() but never directly via .llvm.
    #  So we "can" recognize that grid's dimensions don't change, nor does the length of the
    #  red.rows and blue.rows vectors.
    #  And we are calling Rf_ncols and Rf_nrows in each iteration.
    #
function(grid, rows, cols, n, nr, nc, dir = RED)
{
  ctr = 0L

  for(k in 1:n) {
      i = rows[k]
      j = cols[k]

      if(dir == RED) {
         nextRow = i
	 nextCol = if(j < nc ) j + 1L else 1L
       } else {
         nextCol = j
	 nextRow = if(i < nr ) i + 1L else 1L
       }  

       if(grid[nextRow, nextCol] == EMPTY) {
          ctr = ctr + 1L
          grid[nextRow, nextCol] = dir
	  grid[i, j] = EMPTY
          rows[k] = nextRow
          cols[k] = nextCol
       }
  }
  
  ctr
}

Rf_ncols = ncol
Rf_nrows = nrow

set.seed(1355)
g = convertGrid(createGrid(100, .5))
red.rows = row(g)[g == 1L]
red.cols = col(g)[g == 1L]
blue.rows = row(g)[g == 2L]
blue.cols = col(g)[g == 2L]





if(FALSE) { #XXXX FIX

library(RLLVMCompile)
m = Module()
moveCars_f = compileFunction(moveCars, Int32Type, 
                             list(MatrixType(Int32Type),
                                  INTSXPType, INTSXPType, Int32Type,
                                  Int32Type, Int32Type,
                                  Int32Type),
                             .constants = list(EMPTY = EMPTY, RED = RED, BLUE = BLUE),
                             module = m, .debug = FALSE)

lrunBML_c = compileFunction(runBML, VoidType, 
                            list(MatrixType(Int32Type),
                                 INTSXPType, INTSXPType,
                                 INTSXPType, INTSXPType,
                                 Int32Type),
                            module = m, .debug = FALSE)

ee = ExecutionEngine(m)
tm1 = system.time({o = .llvm(lrunBML_c, g, red.rows, red.cols, blue.rows, blue.cols, 1000L, .all = TRUE, .ee = ee, .duplicate = 1:5)})
tm1 = system.time({o = .llvm(lrunBML_c, g, red.rows, red.cols, blue.rows, blue.cols, 1000L, .all = TRUE, .ee = ee, .duplicate = 1:5)})
plot(o[[1]])

tms = replicate(5, system.time({o = .llvm(lrunBML_c, g, red.rows, red.cols, blue.rows, blue.cols, 1000L, .all = TRUE, .ee = ee, .duplicate = 1:5)}))
print(tms)

}
