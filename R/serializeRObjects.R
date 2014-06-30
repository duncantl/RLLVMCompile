#
# This serializes R objects to a C variable and also restores them
#

saveRObjectAsString =
function(obj)
{
    con = textConnection("foo", "w", local = TRUE)
    on.exit(close(con))
    saveRDS(obj, con, ascii = TRUE)
    paste(textConnectionValue(con), collapse = "\n")
}

loadRObjectFromString =
function(str)
{
  con = textConnection(str)
  readRDS(con)
}
