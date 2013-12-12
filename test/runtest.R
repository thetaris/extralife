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
  
  
  pathNew <- "../test/reports"
  pathRef <- "../test/reports/reference"
  reportFiles <- list.files(path= pathNew, pattern = "^test.*html$")
  
  # compare data from two data.frames and provide a link to diffchecker.com
  compareContents <- function(A, B) {
    if (nrow(A)==nrow(B) && A==B)
      "No change"
    else
      tags$form(action="http://www.diffchecker.com/diff", method="POST",
                tags$div(style="display:none",
                         tags$textarea(name="file1", paste(as.character(A[[1]]),collapse='\n')),
                         tags$textarea(name="file2", paste(as.character(B[[1]]),collapse='\n'))),
                tags$input(type="submit", value="Show differences"))
  }

  # compare report to its reference file, returns a table row object
  compareFiles <- function(filename) {
    file1 <- sprintf('%s/%s',pathNew,filename)
    file2 <- sprintf('%s/%s',pathRef,filename)
    tags$tr(
      tags$td(filename),
      tags$td(tags$a(href=file1, "neuer Report")),
      if (file.exists(file2)) 
        list(
          tags$td(tags$a(href=file2, "Referenz")),
          tags$td(compareContents(read.delim(file1), read.delim(file2)))
        )
      else 
        tags$td("--")
    )
  }
  
  # Construct the full report html file
  report = tags$html(
    tags$head(
      tags$meta(`http-equiv`='Content-Type', content='text/html; charset=utf-8'),
      tags$title('Übersicht der Test-Cases')
      ),
    tags$body(
      tags$h1("Test Ergebnisse"),
      tags$table(
        tags$tr(tags$th("Report"), tags$th("Neu"), tags$th("Referenz"), tags$th("Ergebnis")),
        lapply(reportFiles, compareFiles)
      )
    )
  )
  
  
  if (!is.null(outputfile)){
    outputfilePath = normalizePath(outputfile)
    write(as.character(report), file = outputfilePath)
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
  pathOut <- '../test/reports'
  for (filename in list.files(pathOut, '*.html')) {
    file.remove(sprintf('%s/%s', pathOut, filename))
  }
  print("Old reports deleted.")
  
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
  pathNew <- "../test/reports"
  pathRef <- "../test/reports/reference"
  for (filename in list.files(pathRef, '*.html')) {
    file.remove(sprintf('%s/%s', pathRef, filename))
  }
  for (filename in list.files(pathNew, '*.html')) {
    file.copy(sprintf('%s/%s', pathNew, filename), 
              sprintf('%s/%s', pathRef, filename))
  }
}

