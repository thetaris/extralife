# overwrite Sys.date()
Sys.time <- function() {strptime("09/12/13 11:16:16.683", "%d/%m/%y %H:%M:%OS")}
Sys.Date <- function() {as.Date(Sys.time())}
