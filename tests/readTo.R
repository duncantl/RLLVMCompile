# See Rllvm/explorations/fgets.Rdb for Fgets, etc. and larger context.
readTo =
function(file, n)
{
  ctr = 0L
  tmp = "" # character() # ""
    # perhaps use replicate(n, Fgets(file))
    # and compile that out.  But only want the last value.
  while(ctr < n) {
     tmp = Fgets(file)   
     ctr = ctr + 1L
  }
  tmp
}

