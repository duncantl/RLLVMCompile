guessTypes =
function(x, types)
{
#  vars = all.vars(x)


}

RX =
function(pattern)  
 structure(pattern, class = "RegularExpression")

ReturnTypeTable =
  list(numeric = c("sin", "cos", "log", "sinh", "log", "log10", "logb", "log1p", "exp", "^",
                   'rexp', 'rnorm', 'runif'),
       ScalarNumeric = c('mean', 'median', 'sd', 'var'),
       ScalarInteger = c("length", "floor", 'ceiling'),
       integer = c("rpois", "rbinom"),
       logical = c("duplicated", RX("is.*"), "is"),
       ScalarLogical = c("&&", "||", "any", "all", "%in%"))

# Have to separate mode/typeof and the class
SameModeAsInput =
  c("diag" )


# unlist() is tricky as we only know the type at run-time
# unless we know the types of each of the inputs.

getTypeOf =
function(call, types)
{
  fun = as.character(call[[1]])

  a = sapply(ReturnTypeTable, function(x) fun %in% x)
  if(any(a))
    return(names(ReturnTypeTable)[a])
}

AtomicTypes = 
   c("numeric" = DoublePtrType,
     ScalarNumeric = DoubleType,
     integer = Int32PtrType,
     ScalarInteger = Int32Type
  )

getCTypeOf =
function(rtype)
{
  AtomicTypes[[rtype]]   
}
