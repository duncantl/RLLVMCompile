stop =
function (..., call. = TRUE, domain = NULL, class = character())
{
  if(length(class) == 0)
      base::stop(..., call. = call., domain = domain)
  else
     base::stop(structure(list(message =  .makeMessage(..., domain = domain)),
                          class = c(class, "error", "condition")))
}

.assert =
function(cond, class = character())
{
  if(!cond)
      stop(deparse(cond), class = class)
}
    
