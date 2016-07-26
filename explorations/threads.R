#
# The idea here is to explore creating a routine that
# can be run in multiple threads
# We'll make them run for a reasonable length of time
# We'll use a simple loop
#
#
# How do we marshall an integer as a pointer to the thread.

# Other example: read from several files in parallel.
# Filter a CSV file, counting the number of lines , or counting the number
# of times a particular variable has a value greater than V

# Our routine takes a numeric  vector

data = lapply(c(10, 30, 15, 40, 70)*1000000, rnorm)

threadFun =
function(x)
{
  ans = 0L
  for(val in x)
     ans = ans + (x > .25)
  
}
    

