## vectorize.R
## Move looped, but unvectorized code outside of loops.
## This should *definitely* prompt a compiler warning to let the
## developer know their code is inefficient.
##
## This is like loop-invariant code motion, except the arguments are
## changed to make a vector of the _largest possible size_. Future
## versions should look for loop breaks, and if they are present, make
## a good first guess and allocate in chunks. 

ex.wrong =
function(l, mu = 2, sd = 3) {
  x <- numeric(l)
  x[1] <- rnorm(1, mu, sd)
  for (i in 2:l) {
    x[i] <- x[i-1] + rnorm(1, mu, sd)
  }
  return(x)
}

ex.right =
function(l, mu = 2, sd = 3) {
  x <- numeric(l)
  r <- rnorm(l, mu, sd)
  x[1] <- r[1]
  for (i in 2:l) {
    x[i] <- x[i-1] + r[i]
  }
  return(x)
}


set.seed(0)
system.time(x.wrong <- ex.wrong(10000))

set.seed(0)
system.time(x.right <- ex.right(10000))

stopifnot(x.right == x.wrong)

# Vectorizable functions, with their value being the position of the
# length argument
vectorFunctions <- c(rnorm=1, rbinom=1, sample=2)

findCalls =
# Traverse a block of code, looking for call. Each time it is found,
# the name of the call will be placed in an environment, and its
# arguments in a list.
function(code, call, results=new.env(parent=emptyenv())) {
  for (i in seq_along(code)) {
    if (is.call(code[[i]])) {
      cat("checking:", as.character(code[[i]]), "\n")
      if (as.character(code[[i]][[1]]) %in% call) {
        # put entire call in results
        cat("found:", as.character(code[[i]]), "\n")
        call.name <- as.character(code[[i]][[1]])
        if (exists(call.name, envir=results)) {
          # The call exists; we now want to save all arguments.
          tmp <- get(call.name, envir=results)
          tmp[[length(tmp) + 1]] <- code[[i]]
        } else {
          # call doesn't exist; start a list of arguments
          tmp <- list(code[[i]])
        }
        assign(call.name, tmp, envir=results)
      }
      # Check the arguments
      findCalls(code[[i]][-1], call, results)
    }
  }
  if (length(ls(envir=results)) > 0) {
    out <- list()
    for (item in ls(envir=results)) {
      out[[item]] <- get(item, envir=results)
    }
    return(out)
  }
  return(NULL)
}


## TODO remove isNumericLiteral and isStringLiteral at some point;
## these are duplicated from compile.R, because I don't want to
## source('compile.R') if it's not functioning.
isNumericLiteral =
function(expr) {
  # TODO no complex cases yet
  if (class(expr) %in% c('numeric', 'integer'))
    return(TRUE)
  return(FALSE)
}

isStringLiteral =
# Test whether something is a string literal.
function(expr) {
  return(is.character(expr))
}

isLiteral =
# Check against any type of literal
function(expr)
  return(isStringLiteral(expr) || isNumericLiteral(expr))


examineArgDepends =
# It is absolutely necessary the arguments to any vectorized function
# be static. CodeDepends could be used in later (no proof-of-concept)
# versions. Currently, the arguments are only checked against the
# parameters, or that they are literals
function(call, params) {
  # lazy-eval of params - could break this?
  args <- as.list(call[-1])
  for (arg in args) {
    if (!(isLiteral(arg) || as.character(arg) %in% params)) {
      # TODO: this naively assumes that the parameters are static
      # throughout the body. CodeDepends could fix this.
      return(FALSE)
    }
  }
  return(TRUE)
}

checkVectorizedCodeMotion =
# Check a for loop for code that can be vectorized and moved out of
# it.
function(code, params, len, var) {
  out.expr <- list()
  
  # Look for candidate calls
  cand.calls <- findCalls(code, names(vectorFunctions))

  # With each candidate call, check that arguments are 100% in
  # parameters list, or literals.
  for (call.name in names(cand.calls)) {
    call <- cand.calls[[call.name]]

    # call is now _all_ calls made in this block. We need to deal with
    # each unique one separately, but we'll add this one the simple
    # case works.
    call <- call[[1]] # TODO fix this
    
    is.safe = examineArgDepends(call, params) && call.name %in% names(vectorFunctions)
    browser()
    # Are we not vectorizing?
    # (offset by one because of call name (not just args))
    is.necessary = call[[vectorFunctions[call.name] + 1]] == 1 
    if (is.safe && is.necessary) {
      # Now, find the right argument to expand, grab the maximum
      # length, and prepare the new call to move. We also need to
      # replace the call with a subset.
      cat("Recommended: code motion\n") # better warnings framework?
      cat("  old call:", as.character(call), "\n")
      call[[vectorFunctions[call.name] + 1]] <- quote(NNN) # needs to be unique name
      cat("  new (to be moved) call:", as.character(call), "\n")

      # Now, figure out replacement. This is a rough pass; not robust
      replace.var <- as.name(paste("__", call.name, "__", sep=""))
      replaced <- substitute(rv[i], list(rv=replace.var, i=var))
      cat("  call no to be replaced with:", as.character(replaced), "\n")      
    }
  }
}

checkVectorizedCodeMotion(body(ex.wrong)[[4]][[4]], names(formals(ex.wrong)), 'l', 'i')





if (FALSE) {
  body(ex.wrong)[[4]][[4]][[2]]

  findCalls(body(ex.wrong), "rnorm")

  

}
