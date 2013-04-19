library(RLLVMCompile)
readTo =
function()
{
   tmp = "foo"
   1L
}

#fun = compileFunction(readTo, StringType)
fun = compileFunction(readTo, Int32Type)

readTo =
function()
{
   tmp = character()
   1L
}


readTo =
function()
{
   tmp = 1L
   tmp
}


