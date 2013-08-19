
layout(mat=matrix(c(1,2,3,4,5,3), ncol=2), widths = c(2,1), heights=c(2,2,3))

par(mar = c(1,2.8,1.5,1), bg="white", lwd =0.8)


IncomeBalanceStackedBarPlot(CashTS)
#IncomeBalanceLinePlot(CashTS)

plot.new()


text(x=0, y = 0.9, cex=1.2, adj=c(0,0),labels="Wie hoch ist mein Einkommen?")
text(x=0, y = 0.75, cex=1.2, adj=c(0,0),labels="2300,00 EUR bis 20.10.2013: 6 Wochen Lohnfortzahung (Arbeitgeber)")
text(x=0, y = 0.65, cex=1.2, adj=c(0,0),labels="1800,00 EUR bis 14.04.2015: max. 78 Wochen Krankengeld (Krankenkasse)")
text(x=0, y = 0.55, cex=1.2, adj=c(0,0),labels="1100,00 EUR ab 14.04.2015: ggf. Rente")
text(x=0, y = 0.45, cex=1.2, adj=c(0,0),labels=" 900,00 EUR ab 14.04.2048: staatliche Rente")

par(mar = c(1,1,1.5,1), bg="white")

dial.plot (label = "Sparen", value = 63, dial.radius = 1
           , label.cex = 1.7
           , yellowFrom = 0, yellowTo = 40, yellow.slice.color = "red"
           , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
           , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
)

title(main="2016")


dial.plot (label = "Sparen", value = 33, dial.radius = 1
           , label.cex = 1.7
           , yellowFrom = 0, yellowTo = 40, yellow.slice.color = "red"
           , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
           , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
)

title(main="2048")
