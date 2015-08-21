source("BML.R")

library(RLLVMCompile)
m = Module()
moveCars_f = compileFunction(moveCars, Int32Type, 
                             list(MatrixType(Int32Type), INTSXPType, INTSXPType, Int32Type),  # Int32Type, Int32Type, Int32Type),
                              .constants = list(EMPTY = EMPTY, RED = RED, BLUE = BLUE), module = m, .debug = FALSE)

lrunBML_c = compileFunction(runBML, VoidType, 
                             list(MatrixType(Int32Type), INTSXPType, INTSXPType, INTSXPType, INTSXPType, Int32Type), module = m, .debug = FALSE)

ee = ExecutionEngine(m)
tm1 = system.time({o = .llvm(lrunBML_c, g, red.rows, red.cols, blue.rows, blue.cols, 1000L, .all = TRUE, .ee = ee, .duplicate = 1:5)})
tm1 = system.time({o = .llvm(lrunBML_c, g, red.rows, red.cols, blue.rows, blue.cols, 1000L, .all = TRUE, .ee = ee, .duplicate = 1:5)})
plot(o[[1]])

tms.nopass = tms = replicate(400, system.time({o = .llvm(lrunBML_c, g, red.rows, red.cols, blue.rows, blue.cols, 1000L, .all = TRUE, .ee = ee, .duplicate = 1:5)}))


# Compare this with the code in CaseStudies/BML/BML.Rdb
# The C version seems to be about 12 times faster!!!!
# Okay. This is stupid. It turns out the default values for the parameters
# in crunBML and runBML are 100, not 1000. So there is a factor of 10
# based on the number of iterations!
# However, this version here doesn't return the number of cars that moved in each
# time step. It does compute this in moveCars, but just doesn't store it in
# runBML.
#
# The actual times when adjusted to 100 and not 1000 iterations are
#        user  system elapsed
# LLVM  0.013   0.000   0.012
#       0.008   0.000   0.008
# So the LLVM code is currently 50% slower than the C code.
#
# The code is not exactly the same as the R code. It uses the mutable parameters
# and so pass-by reference. But neverthless it is quite similar.
#
#

#
# Lifting the INTEGER() calls to outside of the moveCars loop moves from about .087
# to .076, so 14%.
# We still do these calls 1000 times in comparison with the C code which is passed
# the nrow & ncol and Rf_length of the different grid and vectors.
# We also call Rf_length on rows 1000 times.
#

