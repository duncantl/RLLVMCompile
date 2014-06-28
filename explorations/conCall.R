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

fc = compileFunction(readUpTo, Int32Type, list(SEXPType),
                      .builtInRoutines = getBuiltInRoutines(atoi = list(Int32Type, StringType)),
                     .CallableRFunctions = list(readLines = list(StringType, list(SEXPType, n = Int32Type))))

if(FALSE) {
con = file("ints", "r")
.llvm(fc, con)
}
