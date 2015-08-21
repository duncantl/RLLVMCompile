source("BML.R")

library(RLLVMCompile)
m = Module()
moveCars_f = compileFunction(moveCars, Int32Type, 
                             list(MatrixType(Int32Type), INTSXPType, INTSXPType, Int32Type),
                              .constants = list(EMPTY = EMPTY, RED = RED, BLUE = BLUE), module = m, .debug = FALSE, optimize = FALSE)

lrunBML_c = compileFunction(runBML, VoidType, 
                             list(MatrixType(Int32Type), INTSXPType, INTSXPType, INTSXPType, INTSXPType, Int32Type), module = m, .debug = FALSE, optimize = FALSE)


mgr = passManager(NULL, FALSE)
passes = list(
 createAggressiveDCEPass,
 createDeadCodeEliminationPass,
 createDeadStoreEliminationPass,
# createInstructionCombiningPass,
 createPromoteMemoryToRegisterPass,
# createDemoteRegisterToMemoryPass,
# createReassociatePass
# createCFGSimplificationPass
# createJumpThreadingPass,
# createTailCallEliminationPass
# createFlattenCFGPass,
# createMemCpyOptPass,
# createCodeGenPreparePass,
 createInstructionSimplifierPass,
# createSinkingPass,
 createStructurizeCFGPass,
 createLoopSimplifyPass,
 createLICMPass,
# createSROAPass,
 createScalarReplAggregatesPass,
 createIndVarSimplifyPass,
 createLoopStrengthReducePass,
# createGlobalMergePass,
 createLoopDeletionPass
# createCorrelatedValuePropagationPass,
# createPartiallyInlineLibCallsPass
    )

invisible(lapply(passes, function(f)  addPass(mgr, f())))

run(mgr, m)

ee = ExecutionEngine(m)

tm1 = system.time({o = .llvm(lrunBML_c, g, red.rows, red.cols, blue.rows, blue.cols, 1000L, .all = TRUE, .ee = ee, .duplicate = 1:5)})
tm1 = system.time({o = .llvm(lrunBML_c, g, red.rows, red.cols, blue.rows, blue.cols, 1000L, .all = TRUE, .ee = ee, .duplicate = 1:5)})

print(tm1)

tms = replicate(5, system.time({o = .llvm(lrunBML_c, g, red.rows, red.cols, blue.rows, blue.cols, 1000L, .all = TRUE, .ee = ee, .duplicate = 1:5)}))[3,]
print(tms)
#plot(o[[1]])

tms1 = replicate(400, system.time({o = .llvm(lrunBML_c, g, red.rows, red.cols, blue.rows, blue.cols, 1000L, .all = TRUE, .ee = ee, .duplicate = 1:5)}))[3,]


# Compare this with the code in CaseStudies/BML/BML.Rdb
# The C version seems to be about 12 times faster!!!!
# And this version here doesn't return the number of cars that moved in each
# time step. It does compute this in moveCars, but just doesn't store it in
# runBML.

# Ways to improve the code
#  Compute INTEGER(rows), INTEGER(cols) just once before the loop.
#  Could define moveCars as taking an int * and int * and give it the dimensions.
#
# The number of cars is fixed so no need to compute length of red.{rows, cols} and blue,{rows,cols}
#
# Instead of getelementtr calculations, just increment by number of  bytes
#   x + sizeof(element)
#
# Registers?
