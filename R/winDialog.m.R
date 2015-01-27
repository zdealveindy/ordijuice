winDialog.m <-
function (type = c("ok", "okcancel", "yesno", "yesnocancel"), message) 
{
# the change is in cancelling checking for interactive use of function
#    if (!interactive()) 
#        stop("winDialog() cannot be used non-interactively")
    type <- match.arg(type)
    res <- .External2(C_winDialog(type, message))
    if (res == 10L) 
        return(invisible(NULL))
    c("NO", "CANCEL", "YES", "OK")[res + 2L]
}

