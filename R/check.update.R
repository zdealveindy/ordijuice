check.update <-
function ()
  {
  new.available <- FALSE
  last.checking <- NULL
  try(load ('updtchck.r'), silent = T)
  if (is.null (last.checking))
    {
        checking <- NULL
        try(checking <- old.packages ('../library', contriburl = 'http://sci.muni.cz/botany/zeleny/R/windows/contrib/R-2.8.1/'), silent = T)
        if (!is.null(checking)) if (checking[,5] > checking[,3]) new.available <- TRUE
        last.checking <- as.numeric(format (Sys.time(), "%y%m%d%H%M%OS1"))
        save (last.checking, file = 'updtchck.r')
     } else
      {
      if (as.numeric(format(Sys.time(), "%y%m%d%H%M%OS1"))-last.checking > 1000000)
        {
        checking <- NULL
        try(checking <- old.packages ('../library', contriburl = 'http://sci.muni.cz/botany/zeleny/R/windows/contrib/R-2.8.1/'))
        if (!is.null(checking)) if (checking[,5] > checking[,3]) new.available <- TRUE
        last.checking <- as.numeric(format (Sys.time(), "%y%m%d%H%M%OS1"))
        save (last.checking, file = 'updtchck.r')
        }
      }
  if (new.available) 
    {
    answ <- winDialog.m ('yesnocancel', 'New update of ORDIJUICE is available online. \nDo you want to update now?')
    if (answ == 'YES') install.libraries ('ordijuice')
    }
}

