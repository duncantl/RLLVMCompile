# From R-exts.texi
#
# Handle rewrites so we don't have to use Rf_length, u
#  understand numeric(n) and that it means a SEXP and not a double[n] here based on the return value.
#  
#
#
convolve =
function(a, b) # , ab)
{
   na = length(a)
   nb = length(b)   
   n = na * nb
   
   ab = numeric(n)
   ab
}

convolve1 =
function(a, b) # , ab)
{
   na = length(a)
   nb = length(b)   
   n = na + nb - 1L
   
   ab = numeric(n)  
   ab.els = REAL(ab)
   a.els = REAL(a)
   b.els = REAL(b)

   for(i in 1:n)
     ab.els[i] = 0


   ctr = 0L
   for(i in 1:na) {
      ab.els[i] = 0
     for(j in 1:nb) {
        #XXX There is a problem here: we want i =1 and b = 1 to map to the first element
        #  of ab.els, but it maps to i + j - 1 since subsetting.
        ab.els[i + j] = ab.els[i + j] + a.els[i] + b.els[j]
        # test basics work: ab.els[i + j - 1L] = a.els[i] * b.els[j]
      }
   }

   ab
#   ctr
}

library(RLLVMCompile)
fc = compileFunction(convolve1, REALSXPType, list(REALSXPType, REALSXPType))
#fc = compileFunction(convolve1, Int32Type, list(REALSXPType, REALSXPType))
.llvm(fc, as.numeric(1:10), as.numeric(1:10))
