'inferType.=' = 'inferType.<-'
function(e, state, ...)
{
  if(is.name(e[[2]]))
      varName = as.character(e[[2]])
  else {
      fnName = as.character(e[[2]][[1]])
      varName = as.character(e[[2]][[2]])
      if(fnName == "[") {

      } else if(fnName == "[[") {

      } else if(fnName == "$") {

      }
  }

  list(varName = varName, subset = )
}
