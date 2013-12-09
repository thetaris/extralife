require(knitr)
# clear workspace

rm(list=ls(all=TRUE))

runtest<-function(testMarkdown, openReport = TRUE){
  dataFiles <- list.files(path="../test/data", pattern = "^test.*json$")
  opts_knit$set(root.dir = getwd())   
  for (testDataFile in dataFiles){
    ELtestDataFile <- sprintf("../test/data/%s",testDataFile)
    testDataFileName = sub(".json", "", testDataFile)
    outputfile <- sprintf("../test/reports/%s_%s.html",testMarkdown, testDataFileName)
    inputfile <- sprintf("../test/%s.Rmd", testMarkdown)
    
    # remember current variables 
    runnerVarsGlobal = ls(all.names =TRUE, envir = .GlobalEnv)
    runnerVarsLocal  = ls(all.names =TRUE)
    
    options(error = function() print(traceback(2)))
    
    knit2html(inputfile, output=outputfile, quiet=TRUE)
    
    #print(traceback())
    
    # remove variables created parsing the test
    newVarsLocal  = ls(all.names =TRUE)
    newVarsGlobal = ls(all.names =TRUE, envir = .GlobalEnv)
    rm(list=newVarsLocal[-which(newVarsLocal %in% runnerVarsLocal)])
    rm(list=newVarsGlobal[-which(newVarsGlobal %in% runnerVarsGlobal)], envir=.GlobalEnv)
    
    # open report
    if (openReport){
      url = sprintf("file:///%s/%s", getwd(), outputfile)
      browseURL(url)
    }
  }
}