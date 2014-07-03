f =
function(x, a)
{
  ans = numeric(length(x))
  for(i in seq(along =x))
      ans[i] = x
  ans
}


g =
function(x, y)
{
  ans = numeric(length(x))
  for(i in seq(along =x)[-1])
      x[i] = y[i-1]*2
  y[1] = sum(x)
  ans
}

h =
function(x, y)
{    
  x[1] = y
  x
}

constInputs =
function(f)
{
  sc = new("Script", as.list(body(f)[-1]))
  inputs = getInputs(sc)
  outs = sapply(inputs, slot, "updates")
  i = match(names(formals(f)), unlist(outs))
  names(formals(f))[is.na(i)]
}

findUnused =
function(f)
{
  sc = new("Script", as.list(body(f)[-1]))
  inputs = getInputs(sc)
  i = sapply(names(formals(f)), findWhenUnneeded, info = inputs)
  names(formals(f))[ is.na(i) ]
}

