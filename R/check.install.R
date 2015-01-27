check.install <-
function (display.spider)
{
## check for correct installation of all libraries
if (!all (c('vegan', 'mgcv', 'ordijuice', 'tcltk', 'tkrgl', 'rgl') %in% .packages(all = T)))
  {
  answ <- winDialog.m ('yesnocancel', 'Some of the R libraries were not installed correctly! \nDo you want to re-install them now? \n(you need to be connected to internet)')
  if (answ == 'YES') install.libraries ('all')
  }

## checks for the version of JUICE and asks for update if it's old
if (is.null (display.spider)) winDialog.m ('ok', 'To get full functionality, you need to install newer version of JUICE!')

## check for the version of R and ask for update if it's old
Rver <- getRversion ()
if (Rver < '2.8.1') winDialog.m ('ok', paste('Currently you are using ', Rver, ' version of R. To ensure the full functionality, please update to version 2.8.1 or newer!', sep = ''))
Rver >= '2.8.1'
}

