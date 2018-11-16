rm(list = ls())
library(ggplot2)
library(plotly)
source("multiplot.R")

Contribution = 450
InitialBalance = 5000


###initializations
t = 12*5

#P = 19983.57+10515.72 #loan
PP = InitialBalance
df <- data.frame(0,InitialBalance,0,0)
names(df) <- c("Month","Balance","Earnings","Total_Earnings")
i = 1 
r = .05794038494 #rate of return
n = 12  #how rate is divided
x = 0
tot_earn = 0


###loop over time
while (PP > 0)
{
  
  #amount after i months
  B <- PP*((1+(r/n))**(i)) + Contribution*(((1 + r/n)**(i)-1)/(r/n))*(1+r/n)
  
  #amount of interest payed on i month
  earnings <- B*(r/n)
  
  x = x + Contribution #this adds up total contributions
  tot_earn = tot_earn + earnings #total interest paid
  
  cf <- data.frame(i,B,earnings,tot_earn)    
  
  names(cf) <- c("Month","Balance","Earnings","Total_Earnings")
  df <- rbind(df,cf)
  i = i + 1
  
  if (i==360) {
    break
  }
  
}
plot_ly(df, x = ~Month,y = ~Balance,type = 'scatter', mode = 'lines',line = list(color = '#4daf4a', width = 2))

plot_ly(df, x = ~Month,y = ~Total_Earnings,type = 'scatter', mode = 'lines',line = list(color = '#377eb8', width = 2))
  
plot_ly(df, x = ~Month,y = ~Earnings,type = 'scatter', mode = 'lines',line = list(color = '#377eb8', width = 2))

  
d <- data.frame(Contribution = numeric(),Total_Balance = numeric())
  

for(i in seq(1, 5000,1)){
  
  B <- 5000*((1+(r/n))**(120)) + i*(((1 + r/n)**(120)-1)/(r/n))*(1+r/n)
  cf <- data.frame(i,B) 
    
  names(cf) <- c("Contribution","Total_Balance")
  d <- rbind(d,cf)
}

plot_ly(d, x = ~Contribution,y = ~Total_Balance,type = 'scatter', mode = 'lines',line = list(color = '#377eb8', width = 2))


# plot_ly(d, x = ~Payment,y = ~Total_Spent,type = 'scatter', mode = 'lines',line = list(color = '#377eb8', width = 2))