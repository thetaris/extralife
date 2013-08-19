

IncomeBalanceLinePlot <- function(CashTS){

library(plotrix)

#net cash flow bar color
net.color <- function(value1,value2){ 
  if(value1 > value2) "green"
  else  "red"
}

#net cash flow vector bar colors
colorVector2 <- function(values1,values2){
  vec = NULL
  for (i in 1: length(values1)){
    vec[i]=net.color(values1[i],values2[i])
  }
  return(vec)
}

#filled between-line area plots

#number of approximation points
np = 1000
y1 <- approx(CashTS$einkommen, n=np)$y
y2<- approx(CashTS$konsum, n=np)$y
CF <- data.frame(A=pmin(y1,y2), M = y1, B=pmax(y1,y2)) 

#fictitial values to supress displaying axis
emptyTicks<-c(rep(0,length(CashTS$einkommen)))
empptyLabels<-c(rep("",length(CashTS$einkommen)))

yLower <- ylimit(as.vector(CashTS$einkommen), as.vector(CashTS$konsum))[1]
yUpper <- ylimit(as.vector(CashTS$einkommen), as.vector(CashTS$konsum))[2]

#cash flow plot
stackpoly(CF, ylim = c(yLower, yUpper),
          xat=emptyTicks,
          xaxlab=empptyLabels, 
          xaxt ="n",yaxt ="n", 
          xlab="",ylab="", 
          mgp = c(1,0.1,0), axis2=F, axis4=F,
          xaxs="i", tcl = -0.5, padj = -1,
          cex.axis = 0.1, lwd=1, 
          border=c("yellow","blue"),
          col=c("white",colorVector2(CashTS$einkommen,CashTS$konsum)))

title(main = "Income", col.main = "blue4", 
      cex.main = 1, font.main = 4)

yAxisCashFlowValues <- seq(yLower,yUpper,(yUpper-yLower)/4)
xAxisTicks <- seq(1,np,by=np/(length(xlabels(CashTS$einkommen))))

axis(1, at = xAxisTicks,labels = xlabels(CashTS$einkommen), 
    xaxs="i", tcl = -0.2, padj = -2,
    col.ticks=4, cex.axis = 0.65, lwd=0.5)
axis(2, at = yAxisCashFlowValues, 
     labels = format(yAxisCashFlowValues,scientific = FALSE),
     las =1, lwd = 0.5,
     yaxs = "r", tcl = -0.2, col.ticks=4, cex.axis = 0.65,
     hadj = 0.5)
legend("topright", bty="n", horiz=TRUE,
       legend =c("Income","Net savings", "Net losses"),
       pch = c(16,16,16), cex=0.48, x.intersp =0.5, y.intersp=0.4,
       col=c("blue4","green","red"))
box(col="dodgerblue")

x1 <- approx(CashTS$balance_assets, n=np)$y
x2<- approx(CashTS$balance_liabilities, n=np)$y
BS <- data.frame(A=pmin(x1,x2), M = x1, B=pmax(x1,x2))
#balance sheet plot
yLower <- ylimit(as.vector(CashTS$balance_assets), as.vector(CashTS$balance_liabilities))[1]
yUpper <- ylimit(as.vector(CashTS$balance_assets), as.vector(CashTS$balance_liabilities))[2]

stackpoly(BS, ylim = c(yLower, yUpper),
          xat=emptyTicks,
          xaxlab=empptyLabels, 
          xaxt ="n",yaxt ="n", 
          xlab="",ylab="", 
          mgp = c(1,0.1,0), axis2=F, axis4=F,
          xaxs="i", tcl = -0.5, padj = -1,
          cex.axis = 0.1, lwd=1, 
          border=c("yellow","blue"),
          col=c("white",colorVector2(CashTS$balance_assets,CashTS$balance_liabilities)))

title(main = "Balance", col.main = "blue4", font.main = 4,
      cex.main = 1)
yAxisBalanceSheetValues <- seq(yLower,yUpper,(yUpper-yLower)/4)
axis(1, at = xAxisTicks,labels = xlabels(CashTS$balance_assets), 
     xaxs="i", tcl = -0.2, padj = -2,
     col.ticks=4, cex.axis = 0.65, lwd = 0.5)
axis(2, at = yAxisBalanceSheetValues, 
     labels = format(yAxisBalanceSheetValues,scientific=FALSE),
     las =1, yaxs = "r", tcl = -0.2, col.ticks=4, cex.axis = 0.65,
     hadj = 0.6, lwd = 0.5)
legend("topright", bty="n", horiz=TRUE,
       legend =c("Assets","Net assets","Net liabilities"),
       pch = c(16,16,16), cex=0.48, x.intersp =0.5, y.intersp=0.4,
       col=c("blue","green","red"))
box(col="dodgerblue")
}
