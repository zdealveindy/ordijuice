install.libraries <-
function  (libs = 'all')
{
  if (libs == 'all') install.packages (pkg=c('vegan', 'mgcv', 'rgl', 'tkrgl', 'tcltk', 'geometry'), repos = "http://cran.r-project.org")
  if (libs == 'ordijuice') {detach (package:ordijuice); install.packages ('ordijuice', contriburl='http://sci.muni.cz/botany/zeleny/R/windows/contrib/R-2.8.1')}
}

