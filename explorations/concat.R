e = quote({
    x = c()
    for(i in z) 
       x = c(x, f(i))
})

f =
function(z)
{    
    x = c()
    for(i in z) 
       x = c(x, f(i))
   x
}

e1 = parse(text = "    x = c()
    for(i in z) {
       x = c(x, f(i))
    }")

# e has length 3 with e[[1]] == "{"
# e1 has length 2



