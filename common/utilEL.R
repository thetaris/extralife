renderEuro <- function(betrag){
  if (is.character(betrag)){
    betrag <- as.numeric(betrag)
  }  
  sprintf("%s €",format(betrag, digits=10, nsmall=2, decimal.mark=",", big.mark="."))
}