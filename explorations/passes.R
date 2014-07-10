library(RLLVMCompile)

diffModule =
function(a, b)
{
   if(is(a, "Module"))    
      a = strsplit(showModule(a, TRUE), "\\n")[[1]]
   if(is(b, "Module"))
      b = strsplit(showModule(b, TRUE), "\\n")[[1]]
   
   c(setdiff(a, b), setdiff(b, a))
}

# This is a place to explore the effects of some of the compiler Pass classes

f =
function(x, y)
{
  a = 1L
  b = 2L
  c = 1L + 2L # XXX  putting x here causes problems!!!!
  b = 3L
  x
}

g =
function()
{

}

m = Module()
fc = compileFunction(f, Int32Type, list(Int32Type, Int32Type), m, optimize = FALSE)

m1 = clone(m)
# Grab original version of the code, unoptimized
str = strsplit(showModule(m1, TRUE), "\\n")[[1]]

#mgr = passManager(NULL, FALSE)
# passManager(m)

p = list(createAggressiveDCEPass, createDeadStoreEliminationPass, createDeadStoreEliminationPass)
lapply(p, function(f) {
            mgr = passManager(NULL, FALSE)
            addPass(mgr, f())
            m = clone(m)
            run(mgr, m)
#            print(showModule(m))
            print(diffModule(m, m1))
       })

#addPass(mgr, createAggressiveDCEPass()) # Doesn't kill off the extra variables.
#addPass(mgr, createDeadStoreEliminationPass()) # This does
#addPass(mgr, createDeadStoreEliminationPass()) # So does this
#Optimize(m, mgr = mgr)
#run(mgr, m)
#showModule(m)
#print(str)
#print(diffModule(m, m1))

    

