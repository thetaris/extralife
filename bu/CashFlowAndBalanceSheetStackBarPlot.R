

IncomeBalanceStackedBarPlot <- function(CashTS){

  # x axis labels
  xlabels <- function(TSvalues){
    labMon = as.vector(cycle(TSvalues))
    labYear = as.vector(floor(time(TSvalues, offset=0)))
    labYearMon = NULL
    
    for( i in 1:length(labYear)){
      labYearMon[i] = paste(labYear[i],labMon[i], sep =".")
    }
    return(labYearMon)
  }
  # y axis limits
  ylimit <- function(value1, value2){
    yRange = NULL
    
    if(min(value2)<0){
      ymin = -10^ceiling(log10(abs(min(value2))))
    }
    else
      ymin = 0
    ymax = 10^ceiling(log10(abs(max(value1))))
    yRange =c(ymin,ymax)
    return(yRange)
  }
  
  #net cash flow bar color
  net_color <- function(value){ 
    if(value > 0) "green"
    else  "red"
  }
  
  #net cash flow vector bar colors
  colorVector <- function(values){
    vec = NULL
    for (i in 1: length(values)){
      vec[i]=net_color(values[i])
    }
    return(vec)
  }
  
  
  #net cash flow bar border color
  net_border_color <- function(value){ 
    if(value >= 0) "blue4"
    else  "red4"
  }
  
  #net cash flow vector bar border colors
  color_border_Vector <- function(value){
    vec = NULL
    for (i in 1: length(value)){
      vec[i]=net_border_color(value[i])
    }
    return(vec)
  }
  
#stacked bar plots

#bar plot of cash flows
yLower <- ylimit(as.vector(CashTS$einkommen),as.vector(CashTS$sparen))[1]
yUpper <- ylimit(as.vector(CashTS$einkommen),as.vector(CashTS$sparen))[2]
CashFlowPlot <-barplot(as.vector(CashTS$einkommen),    
                       ylim = c(yLower, yUpper),
                       col = "white",
                       mgp = c(1,0.1,0), 
                       border = "blue4",
                       yaxt ="n")
#add cash flow savings bar
barplot(as.vector(CashTS$sparen),   
        add=TRUE,
        ylim = c(yLower, yUpper),
        names.arg = xlabels(CashTS$einkommen), 
        col = colorVector(as.vector(CashTS$sparen)),
        mgp = c(1,0.1,0), 
        border = color_border_Vector(as.vector(CashTS$sparen)),
        cex.names = 0.65, yaxt ="n")
title(main = "Income", col.main = "blue4", 
      cex.main = 1, font.main = 4)
xAxisTicks <- CashFlowPlot[seq(1,length(CashFlowPlot), by=1)]

yAxisCashFlowValues <- seq(yLower,yUpper,(yUpper-yLower)/4)
                       
axis(1, at = xAxisTicks,labels = FALSE, xaxs="r", tcl = -0.2, 
     col.ticks=4, cex.axis = 0.65, lwd=0.5)
axis(2, at = yAxisCashFlowValues, 
     labels = format(yAxisCashFlowValues,scientific = FALSE),
     las =1, lwd = 0.5,
     yaxs = "r", tcl = -0.2, col.ticks=4, cex.axis = 0.65,
     hadj = 0.5)
legend("topright", bty="n", horiz=TRUE,
       legend =c("Income","Net savings","Net losses"),
       pch = c(16,16,16), cex=0.48, x.intersp =0.5, y.intersp=0.4,
       col=c("blue4","green","red"))
box(col="dodgerblue")

#bar plot of balance sheet
yLower <- ylimit(as.vector(CashTS$balance_assets),as.vector(CashTS$balance_net))[1]
yUpper <- ylimit(as.vector(CashTS$balance_assets),as.vector(CashTS$balance_net))[2]
BalanceSheetPlot <- barplot(as.vector(CashTS$balance_assets), 
                            beside = FALSE, 
                            ylim = c(yLower, yUpper),
                            col = "white",
                            mgp = c(1,0.1,0), 
                            border="blue4",
                            yaxt ="n")
#add net balance bars
barplot(as.vector(CashTS$balance_net), add = TRUE, 
        ylim = c(yLower, yUpper),
        names.arg = xlabels(CashTS$balance_assets), 
        col = colorVector(as.vector(CashTS$balance_net)),
        mgp = c(1,0.1,0), 
        border=color_border_Vector(as.vector(CashTS$balance_net)),
        cex.names = 0.65, yaxt ="n")

title(main = "Balance", col.main = "blue4", font.main = 4,
      cex.main = 1)
yAxisBalanceSheetValues <- seq(yLower, yUpper,(yUpper-yLower)/4)
axis(1, at = xAxisTicks,labels = FALSE, xaxs="r", tcl = -0.2, 
     col.ticks=4, cex.axis = 0.65, lwd = 0.5)
axis(2, at = yAxisBalanceSheetValues, 
     labels = format(yAxisBalanceSheetValues,scientific=FALSE),
     las =1, yaxs = "r", tcl = -0.2, col.ticks=4, cex.axis = 0.65,
     hadj = 0.6, lwd = 0.5)
legend("topright", bty="n", horiz=TRUE,
       legend =c("Assets","Net assets","Net liabilities"),
       pch = c(16,16,16), cex=0.48, x.intersp =0.5, y.intersp=0.4,
       col=c("blue4","green","red"))
box(col="dodgerblue")

}
