a = quote( a <- f(1, g(x, h(y))))
b = quote( a[[g(x)]] <- f(1, g(x, h(y))))
c = substitute( for(i in g(x)) {  a <- f(1, g(x, h(y))) ; a[[z(x)]] = g(x, h(y))} )
d = substitute( if(g(x) > 10 ) {  a <- f(1, g(x, h(y))) ; a[[z(x)]] = g(x, h(y))} else { a = foo(bar(z)); a[[2]] <- zoo(abc) })

cb =
function()
{
  ctr = 20L
  ans = foo(bob, x = ctr, 1L) # structure(1:10, class = "Jane", names = letters[1:10]))
}
