## Compiler ##
findVar <- function(var, env) {
  return(mget(var, envir=env, ifnotfound=NA))
}

# Make this a generic and have this as the default.
# Allow other people to provide their own.
findCall <- function(call)
{
  op <- match(as.character(call), names(OPS))
  if (is.na(op))
    return(NA) # TODO Some (specified) builtins need to be passed directly,
               # i.e. as.double()
  return(OPS[[op]])
}

checkArgs <- function(args, types, fun) {
  # for side-effect of stop() when wrong arg type
  # encountered. TypeInfo will replace this probably.
  if (length(args) != length(types))
    stop("Wrong number of args provided.")
  for (i in seq_along(args)) {
    if (types[[i]] != 'ANY' && !(typeof(args[[i]]) %in% types[[i]]))
      stop(fun, " received an argument of the wrong type\n",
           "expected any of:\n", paste(types[[i]], sep=', ', collapse=', '),
           "\ngot: ", typeof(args[[i]]))
  }
}

getArgs <- function(expr, env = NULL, ir = NULL) {
  # Converts to list
  if(typeof(expr) != "language")
     stop("expr must be of type 'language' in getArgs")
  ans = sapply(expr[-1], function(x) x)
  if(!is.null(env))
    lapply(ans, compile, env, ir)
  else
    ans
}

isNumericConstant <- function(expr) {
  # TODO no complex cases yet
  if (class(expr) %in% c('numeric', 'integer'))
    return(TRUE)
  return(FALSE)
}


getVariable =
function(sym, env, ir = NULL, load = TRUE, ...)
{

  sym = as.character(sym)
  var = if(exists(sym, env)) {
               # The local variables we create in the function
               # are alloc'ed and so are pointers. They need to be
               # loaded to use their values.
          tmp = get(sym, env)
          if(load && !is.null(ir))
             ir$createLoad(tmp)
          else
            tmp
        } else {
          env$.params[[sym]]
        }
}
