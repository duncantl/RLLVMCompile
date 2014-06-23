rw2d1 =
function(n = 100) {
    xpos = ypos = numeric(n)
    for(i in 2:n) {
          # Decide whether we are moving horizontally or vertically.
      delta = if(runif(1) > .5) 1 else -1
      if (runif(1) > .5) {
        xpos[i] = xpos[i-1] + delta
        ypos[i] = ypos[i-1]
      }
      else {
        xpos[i] = xpos[i-1]
        ypos[i] = ypos[i-1] + delta
      }
    }
    list(x = xpos, y = ypos)
}


library(RLLVMCompile)
fc = compileFunction(rw2d1, SEXPType, list(Int32Type))
