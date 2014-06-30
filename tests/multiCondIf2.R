library(RLLVMCompile)

f = function(x, y)
    {
          ans = 0L
            if(x < 0 || y < 10 || y > 100)  #x + y == 0)
                      ans = 3L
            else
                      ans = 7L

            return(ans)
      }
fc = compileFunction(f, Int32Type, list(x = Int32Type, y = Int32Type))
.llvm(fc, 2, 3)
.llvm(fc, 2, 29)
