renderEuro <- function(betrag){
  if (is.character(betrag)){
    betrag <- as.numeric(betrag)
  } 
  betrag<-round(betrag,2)
  sprintf("%s €",format(betrag, digits=10, nsmall=2, decimal.mark=",", big.mark="."))
}

addBugMuncher <- function(){
  
  return (tags$script('var bugmuncher_options = {
  	language: "en",
		position: "right",
		require_email: false,
		api_key: "c413a62781c1473f2943739d94e472b46dd84036"
	  }; 
  	(function(){ 
  		var node = document.createElement("script"); 
  		node.setAttribute("type", "text/javascript"); 
  		node.setAttribute("src", "//app.bugmuncher.com/js/bugMuncher.min.js"); 
  		document.getElementsByTagName("head")[0].appendChild(node); 
  	})();'
    , type="text/javascript")
  )
}

