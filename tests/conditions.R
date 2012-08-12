library(RLLVMCompile)

f = function(x, y)
{
  ans = 0L
  if(x < 0 || y > 10) 
      ans = 3L
  else
      ans = 7L
 
  return(ans)
}


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

run(fc, -10, 0)
run(fc, 10, 0)
run(fc, 10, 101)
run(fc, 10, 50)  # 7

################

f = function(x, y)
{
  ans = 0L
  if(x < 0 || y > 10 && y <= 100)  #x + y == 0)
      ans = 3L
  else
      ans = 7L
 
  return(ans)
}
fc = compileFunction(f, Int32Type, list(x = Int32Type, y = Int32Type))

run(fc, -10, 40)
run(fc, 10, 40)
run(fc, 10, 400)

############

f = function(x, y)
{
  ans = 0L
  if(x < 0L || y < 10L || x + y > 100L)
      ans = 3L
  else
      ans = 7L
 
  return(ans)
}
fc = compileFunction(f, Int32Type, list(x = Int32Type, y = Int32Type))

run(fc, 3L, 20L)
run(fc, 3L, 98L)
run(fc, 3L, 97L)
