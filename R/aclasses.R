setClass("RTypeDesc", contains = "VIRTUAL")
setClass("DimensionedType", contains = c("RTypeDesc", "VIRTUAL"))
setClass("NativeMatrixType", representation(elType = "ANY", dim = "integer", dimVars = 'character'), contains = "DimensionedType")
setClass("RMatrixType", representation(elType = "ANY", dim = "integer"), contains = c("DimensionedType", "SEXPType"))
#setClass("ArrayType", contains = "RMatrixType")
setClass("DataFrameType", representation(elTypes = "list", nrow = "integer", ncol = "integer"), contains = c("DimensionedType", "SEXPType"))

NativeMatrixType =
function(elType, nrow = NA, ncol = NA, dimVars = character())
{
  if(!missing(ncol))
      dim = c(nrow, ncol)
  else
      dim = nrow

  new("NativeMatrixType", elType = elType, dim = as.integer(dim), dimVars = as(dimVars, "character"))
}

RMatrixType =
function(elType, nrow = NA, ncol = NA, dimVars = character())
{
  if(!missing(ncol))
      dim = c(nrow, ncol)
  else
      dim = nrow

  new("RMatrixType", elType = elType, dim = as.integer(dim))
}

ArrayType =
function(elType, dim = c(NA, NA))
{
  new("ArrayType", elType = elType, dim = as.integer(dim))
}


DataFrameType =
function(elTypes, nrow = NA, ncol = length(elTypes))
{
  if(!missing(ncol))
      dim = c(nrow, ncol)
  else
      dim = nrow
    
  new("DataFrameType", elTypes = elTypes, nrow = as.integer(nrow), ncol = as.integer(ncol))
}


