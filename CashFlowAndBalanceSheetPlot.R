#Cash flow data
Cashflow.income <- c(900,800,400,800,500)
Cashflow.consumption <- c(-850,-850,-550,-750,-450)
Cashflow.savings <- Cashflow.income + Cashflow.consumption
Cashflows <- rbind(Cashflow.income, Cashflow.consumption, Cashflow.savings)
colnames(Cashflows) <- c("July", "August","September", "October","November")
Cashflow.table <- as.table(Cashflows)
Cashflow.color <- c("blue","orange")

#Balance sheet data
Balance.assets <- c(90000,80000,70000,80000,50000)
Balance.liabilities <- c(-40000,-50000,-50000,-40000,-35000)
Balance.net <- Balance.assets + Balance.liabilities
Balancesheet <- rbind(Balance.assets,Balance.liabilities,Balance.net)
colnames(Balancesheet) = colnames(Cashflows)
Balancesheet.table <- as.table(Balancesheet)

#define colors
net.color <- function(value){ 
  if(value >= 0) "green"
  else  "red"
}
net.border.color <- function(value){ 
  if(value >= 0) "green4"
  else  "red4"
}
colorMatrix <- function(value){
  mat = matrix(1:3*length(value), nrow = 3, ncol=length(value))
  for (i in 1: length(value)){
    mat[,i]=c(Cashflow.color,net.color(value[i]))
  }
  return(mat)
}
color.border <- c("blue4","orange4")
color.border.Matrix <- function(value){
  mat = matrix(1:3*length(value), nrow = 3, ncol=length(value))
  for (i in 1: length(value)){
    mat[,i]=c(color.border,net.border.color(value[i]))
  }
  return(mat)
}

#bar plots
layout(matrix(1:2, ncol=1))

par(mar = c(2,2.5,1.5,1))
#bar plot of cash flows
CashFlowPlot <-barplot(Cashflow.table, beside = TRUE,  ylim = c(-1000, 1000),
         names.arg = colnames(Cashflows), 
         col = colorMatrix(Cashflow.savings),
         mgp = c(1,0.1,0), 
        border = color.border.Matrix(Cashflow.savings),
        cex.names = 0.7, yaxt ="n")

title(main = "Cash flows", col.main = "blue4", 
      cex.main = 1, font.main = 4)
xAxisTicks <- CashFlowPlot[seq(2,length(CashFlowPlot), by=3)]
yAxisCashFlowValues <- seq(-1000,1000,200)
axis(1, at = xAxisTicks,labels = FALSE, xaxs="r", tcl = -0.2, 
     col.ticks=4, cex.axis = 0.6, lwd=0.5)
axis(2, at = yAxisCashFlowValues, 
     labels = format(yAxisCashFlowValues,scientific = FALSE),
     las =1, lwd = 0.5,
     yaxs = "r", tcl = -0.2, col.ticks=4, cex.axis = 0.6,
     hadj = 0.5)
legend("bottomright", bty="n", horiz=TRUE,
       legend =c("Income", "Consumption","Net savings"),
       pch = c(16,16,16), cex=0.48, x.intersp =0.5, y.intersp=0.4,
       col=colorMatrix(Cashflow.savings))
box(col="dodgerblue")

#bar plot of balance sheet
BalanceSheetPlot <- barplot(Balancesheet.table, beside = TRUE, ylim = c(-100000, 100000),
        names.arg = colnames(Balancesheet), 
        col = colorMatrix(Balance.net),
        mgp = c(1,0.1,0), 
        border=color.border.Matrix(Balance.net),
        cex.names = 0.7, yaxt ="n")

title(main = "Balance sheet", col.main = "blue4", font.main = 4,
      cex.main = 1)
yAxisBalanceSheetValues <- seq(-100000,100000,20000)
axis(1, at = xAxisTicks,labels = FALSE, xaxs="r", tcl = -0.2, 
     col.ticks=4, cex.axis = 0.6, lwd = 0.5)
axis(2, at = yAxisBalanceSheetValues, 
     labels = format(yAxisBalanceSheetValues,scientific=FALSE),
     las =1, yaxs = "r", tcl = -0.2, col.ticks=4, cex.axis = 0.6,
     hadj = 0.6, lwd = 0.5)
legend("bottomright", bty="n", horiz=TRUE,
       legend =c("Assets", "Liabilities","Net balance"),
       pch = c(16,16,16), cex=0.48, x.intersp =0.5, y.intersp=0.4,
       col=colorMatrix(Balance.net))
box(col="dodgerblue")
