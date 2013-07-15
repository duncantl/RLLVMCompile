# See /Users/duncan/Classes/StatComputing/Book/Simulation/BML/BML.R
# for vectorized version

createGrid =
function(n = c(15, 15), r = 10, c = 11, prob = c(red = .5, blue = .5),
          density = NA, exact = FALSE)
{
   # if we have only on n, and it is a proportion
   # then turn it into a count as a proportion of 
   # the grid size
  if(length(n) == 1 && n < 1)
     n = r * c * n

  if(!missing(density)) 
     n = r * c * density

     # if we have only one count, then replicate it 
     # that many times and allow the last grouping to pick up 
     # any slack.    
  if(length(n) == 1 && length(colors) > 1)  {
       tmp = rep(floor(n/(length(colors)-1)), length(colors)-1)
       n = c(tmp, n - as.integer(sum(tmp)))
  }

  locations = expand.grid(1:r, 1:c)
  locations = locations[sample(1:nrow(locations), sum(n)), ]
  
  locations$carType = if(!exact) 
                          factor(sample(names(prob), sum(n), prob = prob, replace = TRUE), levels = names(prob))
		      else 
                          factor(rep(names(prob), n), levels = names(prob))


  G = matrix(0L, r, c)
  G[ cbind(locations[, 1], locations[,2]) ] = as.integer(locations$carType)
  class(G) = "Grid"

  structure(list(locations = locations, G = G), class = "BMLGrid")
}


plot.BMLGrid =
function(x, y, ..., addGrid = FALSE)
{
  plot(0, 0, type = "n", 
       xlim = c(0, ncol(x$G)), ylim = c(0, nrow(x$G)), 
       axes = FALSE, xlab = "columns", ylab = "rows", ...) # , pty = "s", ...)

  axis(1)
  r = nrow(x$G)
  tmp = pretty(1:r)
  axis(2, at = tmp, labels = rev(tmp))
  box()


   # While we may want to loop, rect is vectorized
   # and it is actually simpler to write in this form
   # since we merely omit the for loop and indices
  rect(x$locations[, 2] - 1, r - x$locations[, 1],  
       x$locations[, 2], r - x$locations[, 1] + 1, 
       border = NA,
       col = as.character(x$locations$carType)) # , density = 0) # density = NA is very slow.

  if(addGrid) {
      abline(v = 0:ncol(x$G))
      abline(h = 0:nrow(x$G))
  }
}

