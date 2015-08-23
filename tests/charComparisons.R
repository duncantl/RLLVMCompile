library(RLLVMCompile)

# This function maps a string to a level
# We know the levels are
#  bob, boy, jane, jill, zoe
# No other possible values are possible.
# See the CompilerTechonlogiesInR book - CaseStudies/csv.xml

getLevel =
function(token)
{
  if(token[1] == 'z')
     return(5L)

  if(token[1] == 'b') {
     if(token[3] == 'b')
         1L
     else
         2L
  } else if(token[2] == 'a')  {
         3L
  } else
         4L
}

fc = compileFunction(getLevel, Int32Type, list(StringType))

.llvm(fc, "zoe")
.llvm(fc, "bob")
.llvm(fc, "boy")
.llvm(fc, "jill")
.llvm(fc, "jane")

.llvm(fc, "xxx")  # 4

