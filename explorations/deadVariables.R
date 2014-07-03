library(CodeDepends)

f =
function(a, b)
{
  x = a*10
  foo(x)
  z = b*20
  z
}

i = getInputs(f)

g = makeVariableGraph(info = i) 
plot(g)  # This seems wrong.



script = "
  u = 10
  x = a*10
  foo(x)
  x[1] = 10
  z = b*20
  z
"

e = parse(text = script)
sc1 = new("Script", e)
sc = readScript(txt = script)

inp1 = getInputs(sc1)
inp = getInputs(sc)

if(FALSE) {
sc = readScript("../explorations/script.R")
inp = getInputs(sc)
}

findWhenUnneeded("x", , inp)
findWhenUnneeded("x", , inp1)

findWhenUnneeded("u", , inp) # never needed so NA.


any(sapply(inp, function(node) "x" %in% node@updates)) # true

sapply(inp, function(node) "z" %in% node@updates) # But z is never updated.



