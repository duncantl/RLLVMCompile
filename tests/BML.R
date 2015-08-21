EMPTY = 0L
RED = 1L
BLUE = 2L

createGrid = 
function(dims = c(100, 100), numCars = .3)
{
   if(length(dims) == 1)
     dims = rep(dims, 2)

   if(length(numCars) == 1 && numCars < 1)
      numCars = rep(prod(dims) * numCars/2, 2)

   grid = matrix("", dims[1], dims[2])

   pos = sample(1:prod(dims), sum(numCars))
   grid[pos] = sample(rep(c("red", "blue"), 
                          ceiling(numCars)))[seq(along = pos)]

   grid
}

convertGrid =
    #
    # This version converts the grid from "red" "blue" "" elements
    #  to  1 2 0 elements
    #
function(g)
{
  g[g == "red"] = 1L
  g[g == "blue"] = 2L
  g[g == ""] = 0L
  g = matrix(as.integer(g), nrow(g), ncol(g))
  class(g) = c("BMLGrid", "matrix")
  g
}

plot.BMLGrid =
function(x, ...)
{
   if(typeof(x) == "character")
     z = matrix(match(x, c("", "red", "blue")), nrow(x), ncol(x))
   else
     z = x
   image(t(z), col = c("white", "red", "blue"),  #XXX [, nrow(z):1]
          axes = FALSE, xlab = "", ylab = "", ...)
   box()
}

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
   for(i in 1:niter) {
      moveCars(g, rrows, rcols, RED)
      moveCars(g, brows, bcols, BLUE)
   }
}

moveCars =
    #
    #  When we know we are compiling runBML() and moveCars() together
    #  and we are calling moveCars() from runBML() but not directly,
    #  we "can" recognize that grid's dimensions don't change
    # and we are calling Rf_ncols and Rf_nrows in each iteration.
    #
function(grid, rows, cols, dir = RED)
{
  nc = Rf_ncols(grid)
  nr = Rf_nrows(grid)
  ctr = 0L

  for(k in 1:length(rows)) {
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

if(FALSE)
 tm.rr = system.time({ o =  runBML(g, red.rows, red.cols, blue.rows, blue.cols, 100L) })




