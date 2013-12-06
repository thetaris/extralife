require(knitr)
# clear workspace

rm(list=ls(all=TRUE))

runtest<-function(testMarkdown){
  dataFiles <- list.files(path="../test", pattern = "^test.*json$")
  opts_knit$set(root.dir = getwd())   
  for (testDataFile in dataFiles){
    ELtestDataFile <- sprintf("../test/%s",testDataFile)
    testDataFileName = sub(".json", "", testDataFile)
    outputfile <- sprintf("reports/testMarkdown_%s.html",testDataFileName)
    
    # remember current variables 
    runnerVars = ls(all.names =TRUE)
    knit2html(testMarkdown, output=outputfile)
    
    # remove variables created parsing the test
    newVars = ls(all.names =TRUE)
    rm(list=newVars[-which(newVars %in% runnerVars)])
    
    # open report
    url = sprintf("file:///%s/%s", getwd(), outputfile)
    browseURL(url)
  }
}