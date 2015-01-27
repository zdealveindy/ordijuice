install.libraries <- function  (libs)
{
  install.packages (pkg = not.installed)
  if (libs == 'ordijuice') {detach (package:ordijuice); install.packages ('ordijuice', contriburl='http://sci.muni.cz/botany/zeleny/R/windows/contrib/R-2.8.1')}
}

