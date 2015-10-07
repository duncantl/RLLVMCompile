# Description:
#   Handler function for .byref().

# NOTE: Rather than using .byref() to mark indexing operations that should be
# pointers, we could have the compiler match on parameter types of functions
# being called. That is, if the indexed variable is used in a routine that
# expects a pointer, create one; otherwise create a concrete copy.

# NOTE: sometimes we can lift things out of loops.

byrefHandler = function(call, env, ir, ...)
{
  # Extract argument of .byref().
  arg = call[[2]]

  is_unsupported =
    as.character(arg[[1]]) != '[' ||
    length(arg) != 4 ||
    as.character(arg[[3]]) == "" ||
    as.character(arg[[4]]) != ""

  if (is_unsupported)
    stop(".byref() only supports the form `x[i, ]`.")

  # NOTE: consolidate with other subset handler code.
  # Get variable.
  varName = as.character(arg[[2]])
  var = getVariable(varName, env, ir, load = FALSE)

  # Get index.
  idx = arg[[3]]
  if (is.numeric(idx))
    idx = as.integer(idx)
  # FIXME: this doesn't respect .zeroBased.
  idx = subtractOne(idx)
  idx = compile(idx, env, ir, isSubsetIndex = TRUE)
  idx = createSExt(ir, idx, Int64Type)

  # Generate a GEP.
  #block = getInsertBlock(ir)
  #browser()

  gep = createGEP(ir, var, idx)

  return(gep)
}
