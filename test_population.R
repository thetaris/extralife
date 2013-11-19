age = c(1:20)*5
age = c(age,age)      # male and female
age = c(age,age, age) # education, working, retired

phase[1:40] = "education"
phase[41:80] = "working"
phase[81:120] = "retired"

forecast = abs(rnorm(120))

forecast[21:40] = -forecast[21:40]
forecast[61:80] = -forecast[61:80]
forecast[101:120] = -forecast[101:120]

forecast[6:20] = 0  # phase education: set working and retired to zero
forecast[26:40] = 0  # phase education: set working and retired to zero

forecast[41:45] = 0 # phase working: set education and retired to zero
forecast[54:60] = 0

forecast[61:65] = 0 # phase working: set education and retired to zero
forecast[74:80] = 0

forecast[81:93] = 0 # phase retired: set working and education to zero
forecast[101:113] = 0 # phase retired: set working and education to zero

rplotData = data.frame(age, phase, forecast)

pf<-nPlot(forecast~age, group="phase",
          data = rplotData, 
          type = "multiBarHorizontalChart"
          
)


pf$chart(stacked = 'true')

pf$chart(tooltipContent = "#! function(key, x, y) { 
           return  '<h3>' + key + '</h3>'  +
           '<p>'+'age ' + x + '</p>'+
           '<p>'  + y + 
           ' population'+'</p>'           
           
} !#")



pf$chart(color= c('#ff7f0e','#1f77b4','#d62728'))

pf$yAxis(showMaxMin=FALSE)

pf
