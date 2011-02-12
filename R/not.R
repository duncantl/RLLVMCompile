# not.R - handling of unary !. This could possibly be merged into
# logicOpHandler.R

notHandler =
# This is a unary operator, so we should expect only one argument.
# This needs to handle vectors and matrices; so far just scalars.
function(call, env, ir, ...) {
  if (length(call) != 2) # handle with checkArgs
    stop("Unary operator ! requires only one argument.")

  # TODO This is from mathHandler.R, so this could possibly be turned
  # into a more general method.
  x <- call[[2]]
  
  if (is(x, "numeric")) {
    if(identical(getMathOpType(getType(x)), Int32Type))
      x <- createIntegerConstant(as.integer(x))
    else
      x <- createDoubleConstant(as.numeric(x))
  } else if(is.name(x)) {
    # Potentially have to cast based on the target type
    x <- getVariable(x, env, ir)
  } else
  x <- compile(x, env, ir, ...)
  
  ins = ir$binOp(Xor, x, createIntegerConstant(1L))
  ins
}
  
