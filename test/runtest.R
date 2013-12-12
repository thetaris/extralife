require(knitr)
# clear workspace should occure here:
# rm(list=ls(all=TRUE))

runtest<-function(testMarkdown, openBrowser = TRUE){
  # runtest(testMarkdown, openBrowser = TRUE)
  #   creates the Knit2Html from MarkdownFile testMarkdown in
  #   "extralife/test" subfolder
  #
  # example:
  #  
  # runtest("test_mortality") # note: no extension ".Rmd"
  #
  dataFiles <- list.files(path="../test/data", pattern = "^test.*json$")
  opts_knit$set(root.dir = getwd())   
  for (testDataFile in dataFiles){
    # set value of ELtestDataFile for usage in report
    ELtestDataFile <- sprintf("../test/data/%s",testDataFile)
    
    testDataFileName = sub(".json", "", testDataFile)
    outputfile <- sprintf("../test/reports/%s_%s.html",testMarkdown, testDataFileName)
    inputfile <- sprintf("../test/%s.Rmd", testMarkdown)
    
    # remember current variables 
    runnerVarsGlobal = ls(all.names =TRUE, envir = .GlobalEnv)
    runnerVarsLocal  = ls(all.names =TRUE)
    
    knit2html(inputfile, output=outputfile, quiet=TRUE, envir=new.env())
    
    # remove variables created parsing the test
    newVarsLocal  = ls(all.names =TRUE)
    newVarsGlobal = ls(all.names =TRUE, envir = .GlobalEnv)
    rm(list=newVarsLocal[-which(newVarsLocal %in% runnerVarsLocal)])
    rm(list=newVarsGlobal[-which(newVarsGlobal %in% runnerVarsGlobal)], envir=.GlobalEnv)
    
    # open report
    if (openBrowser){
      url = sprintf("file:///%s/%s", getwd(), outputfile)
      browseURL(url)
    }
  }
}

compareReports<-function(outputfile="testcases_overview.html", openBrowser = TRUE){
  # compareReports(outputfile="testcases_overview.html", openBrowser = TRUE)
  #   compares all "extralife/test/reports/test*.html" files with the references under
  #                "extralife/test/reports/reference/test*.html"
  #   and creates a HTML summarry outputfile  
  #
  # example:
  #
  # compareReports()
  
  decodeFCStatus<-function(cmdString){    
    suppressWarnings({
      tmpStatus <- shell(cmd=sprintf("%s 2>nul 1>nul",cmdString), intern=FALSE, wait=TRUE)
      status = "ok"
      color = "#00FF00"
      reportDiff = NULL
      if (tmpStatus==1){
        status <- "Files do not match."
        color = "#FF0000"
        reportDiff = shell(cmd=cmdString, intern=TRUE)
      }else if (tmpStatus==2){
        status <- "File not found."
        color = "#FFFF00"
      }else if (tmpStatus!=0){
        status <- "Error."
        color = "#FF0000"
        reportDiff = shell(cmd=cmdString, intern=TRUE)
      }        
      res = list()
      res$status <- status
      res$color <- color
      res$reportDiff <- reportDiff
      return(res)
    })
  }
  
  referenceFiles <- list.files(path="../test/reports/reference", pattern = "^test.*html$")
  reportFiles <- list.files(path="../test/reports", pattern = "^test.*html$")
  
  result = sprintf("<html> 
                    <head>
                    <meta http-equiv='Content-Type' content='text/html; charset=utf-8'/>
                    <title>Übersicht der Test-Cases</title>
                    <body>
                    <table>")
  
  for (iterReport in reportFiles){    
    fileReport    <- normalizePath(sprintf("../test/reports/%s", iterReport))
    fileReference <- normalizePath(sprintf("../test/reports/reference/%s", iterReport))
    cmdString <- sprintf("FC /A /W %s %s", fileReport, fileReference)
    #reportDiff<-suppressWarnings(shell(cmdString, intern=TRUE))
    diffRes<-decodeFCStatus(cmdString)
    
    
    urlReport = sprintf("file:///%s", fileReport)
    urlReference = sprintf("file:///%s", fileReference)
    result = sprintf("%s\n<tr>
                            <td>
                              <strong style='background-color:%s'>%s</strong>  
                              <a href='%s'>
                                [neuer Report]
                              </a>
                              <a href='%s'>
                                [Referenz]
                              </a>
                                %s
                              </td>
                          </tr>", result, diffRes$color, iterReport, urlReport, urlReference, diffRes$status)
    if ((diffRes$status == "ok")|(diffRes$status == "File not found.")){
      result = sprintf("%s\n\n<tr>\n<td><br/></td>\n</tr>",result)
    }else{
      result = sprintf("%s\n\n<tr>\n<td><textarea cols='80' rows='4'>", result)
      for (iterReportDiff in diffRes$reportDiff[2:length(diffRes$reportDiff)]){
        result = sprintf("%s\n%s", result, iterReportDiff)
      }    
      result = sprintf("%s\n</textarea></td>\n</tr>", result)
    }
  }
  result = sprintf("%s\n
                   </table>\n
                   </body>\n
                   </head>\n
                   </html>", result)
  if (!is.null(outputfile)){
    outputfilePath = normalizePath(outputfile)
    write(result, file = outputfilePath)
    if (openBrowser){
      url = sprintf("file:///%s", outputfilePath)
      browseURL(url)
    }
  }
  #cat(result)
}

runtestAll<-function(outputfile="testcases_overview.html", openBrowser = TRUE){
  # runtestAll(outputfile="testcases_overview.html", openBrowser = TRUE)
  #   runtestAll calles runtest for each "test.Rmd" file under "extralife/test"
  #   and finally calles compareReports() to present differences to the reference
  #   values from previous calls
  #
  # example:
  # 
  #   runtestAll()
  cmdString <- sprintf("del /Q ..\\test\\reports\\*.html")  
  res <- shell(cmdString)
  if (res=="0"){
    print("Old reports deleted.")
  } else {
    print(sprintf("Old reports not deleted: %s",res))  
  }
  
  testFiles <- list.files(path="../test", pattern = "^test.*Rmd$")
  for (iterTestFile in testFiles){
    print(sprintf("Running Testcase: %s", iterTestFile))
    runtest(sub(".Rmd","",iterTestFile), openBrowser = FALSE)
  }
  print("Creating summary of comparison.")
  compareReports(outputfile, openBrowser)
  print("Done.")
  
}

resetReference<-function(){
  # resetReference()
  #   resetReference() deletes all files under "extralife/test/reports/reference" and
  #   copies all new report files from "extralife/test/reports/*.html" to 
  #   "extralife/test/reports/reference". Thus, new values for the reference are set.
  #
  # example:
  # 
  # resetReference()  
  cmdString <- sprintf("del /Q ..\\test\\reports\\reference\\*.html")  
  print((shell(cmdString, intern=TRUE)))
  cmdString <- sprintf("copy ../test/reports/*.html ../test/reports/reference")
  print(suppressWarnings(shell(cmdString, intern=TRUE, translate=TRUE)))
}

