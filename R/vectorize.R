## vectorize.R
## Move looped, but unvectorized code outside of loops.
## This should *definitely* prompt a compiler warning to let the
## developer know their code is inefficient.
##
## This is like loop-invariant code motion, except the arguments are
## changed to make a vector of the _largest possible size_. Future
## versions should look for loop breaks, and if they are present, make
## a good first guess and allocate in chunks. 


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
    
    is.safe <- examineArgDepends(call, params) && call.name %in% names(vectorFunctions)
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
      
      # needs to be unique name
      new.call <- call
      new.call[[vectorFunctions[call.name] + 1]] <- len
      cat("  new (to be moved) call:", as.character(call), "\n")

      # Now, figure out replacement. This is a rough pass; not robust
      replace.var <- as.symbol(paste("_", call.name, "_vector_", sep=""))
      replaced <- list(call, substitute(rv[i], list(rv=replace.var, i=var)))
      cat("  call no to be replaced with:", as.character(replaced), "\n")
      replace <- TRUE
      # We break now, because better handling for multiple call
      # replaces is needed
      break()
    }
  }
  if (replace)
    return(list(new.call, replaced))
  return(FALSE)
}




getLimits =
# See comments in createLoop.R - this is duplicated. TODO remove duplication
function(call)
{
  op = as.character(call[[1]])

  ans = if(op == ":") {
           list(from = call[[2]], to = call[[3]])
         } else if(op == "seq_along") {
           tmp = substitute(length(x), list(x = call[[2]]))
           list(from = 1L, to = tmp)
         } else if(op == "seq") {
           k = match.call(seq, call)
           argNames = names(k)[-1]
                                        # formals(seq) returns ...
           formals = c("from", "to", "by", "length.out", "along.with")
           i = argNames == ""
           argNames[i] = formals[which(i)]
           structure(as.list(call[-1]), names = argNames)
         }

    # how should we get integers when we have, e.g., 1:10 which are
    # numeric
  ans = lapply(ans, function(val) if(is.numeric(val) && val == as.integer(val)) as.integer(val) else val)

  ans
}


replaceCalls =
# Replace all calls with varied code. Right now this is recursive which
# breaks the code replacement. TODO - non-recursive version?
function(code, call, replace) {
  for (i in seq_along(code)) {
    if (is.call(code[[i]])) {
      if (code[[i]] == call) {
        cat("replacing:", as.character(code[[i]]), "with:", as.character(call), "\n")
        code[[i]] <- replace # broken
      }
      replaceCalls(code[[i]][-1], call, replace)      
    }
  }
  return(code)
}


replaceVectorizedCodeMotion =
# After checking for possible code motion, replace if necessary
function(for.code, params) {
  var <- for.code[[2]]
  len <- getLimits(for.code[[3]])$to # consider from?
  motion <- checkVectorizedCodeMotion(for.code, params, len, var)
  if (motion != FALSE) {
    # Code motion needed, unpack results
    move.out <- motion[[1]]
    call.search <- motion[[2]][[1]]
    call.replace <- motion[[2]][[2]]
  }
  
  
}





if (FALSE) {
  body(ex.wrong)[[4]][[4]][[2]]

  findCalls(body(ex.wrong), "rnorm")

  

}
