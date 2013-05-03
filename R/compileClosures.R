#
# The idea here is to compile one or more functions that
# use lexical scoping into a module.
# We'll use one of the collector functions in RCIndex/RClangSimple
# These functions return a list of functions, typically with 
# "update" and 'results' elements. 
# We can call this generator function and get an instance of the
# closures. We can examine the environment(s) of the function
# and find the global variables. We can also examine the functions
# to find <<- assignments.
# How do we "compile" these closures. One approach is to
# create global variables in the module to represent these
# closure variables in an obvious ways.  The the routines we
# generate would modify those variables.
# Some routines to collect the results would access these
#
#  z = genCallCollector()
#  names(z)
#  
#
