getSEXPTypeElementAccessor =
function(type)
{
   if(is(type, "INTSXPType") || is(type, "LGLSXP")) 
      "INTEGER"
   else if(is(type, "REALSXPType")) 
      'REAL'
   else
      stop("not done yet")
}
