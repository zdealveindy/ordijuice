write.warnings <-
function (title.warnings)
  {
  temp.warnings <- NULL
  for (i in seq (1,length (title.warnings))) temp.warnings <- append (temp.warnings, paste (title.warnings[i], '\n', sep = ''))
  winDialog.m ('ok', paste(temp.warnings, collapse = ''))
  }

