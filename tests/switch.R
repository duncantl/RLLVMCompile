library(RLLVMCompile)

f =
function(i)
{
     switch(i,
            '2' = 12,
            '3' = i * 1.5,
            '8' = i/2)

}
