library(RLLVMCompile)

# The idea is to have a callback to R for an aspect we cannot (legitimately)
# see from the R API.
#
# The idea is to use readLines to read one line at a time.
# The data are just lines with one integer value per line.
# When we see a 99, we terminate the loop and stop reading.
#

# How do we specify readLines() needs to be called through R
#  that its return type is a string so txt is a char *
#

readUpTo =
function(con)
{
  ctr = 0L

  while(TRUE) {

      txt = readLines(con, n = 1)
      ctr = ctr + 1L
      i = atoi(txt)
      if(i == 99)
          break
  }
  
  ctr
}

# Make a connection type class.

m = Module()
ee = ExecutionEngine(m)
fc = compileFunction(readUpTo, Int32Type, list(SEXPType), module = m,
                     .builtInRoutines = getBuiltInRoutines(atoi = list(Int32Type, StringType)),
                     .CallableRFunctions = list(readLines = list(StringType, list(SEXPType, n = Int32Type))),
                     .ee = ee)

if(TRUE) {
con = file("ints", "r")
.llvm(fc, con, .ee = ee)


# now for speed comparisons
if(!file.exists("testConCall.data")) {
    set.seed(3L)
    N = 1e7
    x = rpois(N, 3)
    cat(x, 99, sep = "\n", file = "testConCall.data")
}

#  Not great speed. Is it the implicit strdup().
#  We also need to free that.
con = file("testConCall.data", "r")
tm.llvm = system.time(.llvm(fc, con, .ee = ee))

con = file("testConCall.data", "r")
tm.r = system.time(rf(con))

con = file("testConCall.data", "r")
tm.rvec = system.time(rf.fast(con))
}

rf =
function(con)
{
  ctr = 0L

  while(TRUE) {

      txt = readLines(con, n = 1)
      ctr = ctr + 1L
      i = as.integer(txt)
      if(i == 99)
          break
  }
  
  ctr
}

rf.fast =
function(con)
{
  which(as.integer(readLines(con)) == 99)
}
