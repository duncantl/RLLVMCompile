setClass("RTypeDesc", contains = "VIRTUAL")
setClass("DimensionedType", contains = c("RTypeDesc", "VIRTUAL"))
setClass("MatrixType", representation(elType = "ANY", dim = "integer"), contains = "DimensionedType")
#setClass("ArrayType", contains = "MatrixType")
setClass("DataFrameType", representation(elTypes = "list", nrow = "integer", ncol = "integer"), contains = "DimensionedType")

MatrixType =
function(elType, nrow = NA, ncol = NA)
{
  if(!missing(ncol))
      dim = c(nrow, ncol)
  else
      dim = nrow

  new("MatrixType", elType = elType, dim = as.integer(dim))
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
    
  new("ArrayType", elType = elTypes, dim = as.integer(dim))
}


