check.install <- function (display.spider)
{
## check for correct installation of all libraries
libraries.needed <- c('vegan', 'mgcv', 'ordijuice', 'tcltk', 'tkrgl', 'rgl', 'geometry')
not.installed <- libraries.needed [!libraries.needed %in% .packages(all = T)]
if (length (not.installed) > 0)
  {
  answ <- tclvalue (tkmessageBox (type = 'yesnocancel', message = 'Some of the R libraries were not installed correctly! \nDo you want to re-install them now? \n(you need to be connected to internet)'))
  if (answ == 'yes') install.libraries (not.installed)
  }

## checks for the version of JUICE and asks for update if it's old
if (is.null (display.spider)) tkmessageBox (type = 'ok', message = 'To get full functionality, you need to install newer version of JUICE!')

## check for the version of R and ask for update if it's old
Rver <- getRversion ()
if (Rver < '2.8.1') tkmessageBox (type = 'ok', message = paste('Currently you are using ', Rver, ' version of R. To ensure the full functionality, please update to version 2.8.1 or newer!', sep = ''))
#Rver >= '2.8.1'
}

