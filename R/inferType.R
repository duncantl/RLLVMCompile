## Created this to not interfere with guessType.R, although the goals
## may be different.
##
##

coercionOrder = c(DoubleType, Int32Type) # much is needed

inferType =
# In assignment the type cannot be easily gathered with getType,
# because it's often of class Value. Instead the entire call needs to
# be inspected.
# First pass: only one call (not multiples)
function(call, env) {
  vars <- all.vars(call)
  op <- call[[1]]
  if (as.character(op) %in% MathOps) {
    var.types <- sapply(vars, function(x) getType(x, env))
    found.types <- match(var.types, coercionOrder)

    if (any(is.na(found.types)))
      stop("Type not found in coercionOrder vector.")
    if (length(unique(found.types)) > 1)
      warning("Coercing types - not implemented yet") # How can we do
                                                      # this here?
    
    type <- coercionOrder[[min(found.types)]]
    return(type)
  } else
    stop("inferType only works on MathOps now.")
}
