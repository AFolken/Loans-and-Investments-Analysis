rm(list = ls())
library(ggplot2)
library(plotly)
source("multiplot.R")

StudentLoan_Func = function(payment,Loan){
  ###initializations
  t = 12*5
  
  #P = 19983.57+10515.72 #loan
  PP = Loan
  df <- data.frame(0,Loan,0,0,0)
  names(df) <- c("Month","Loan","Interest","Total Spent","Total Interest")
  i = 1 
  r = .05794038494#loan interest rate
  n = 12  #how interest rate is divided
  x = 0
  tot_int = 0
  
  
  ###loans loop over time
  while (PP > 0)
  {
    
    #amount after i months
    PP <- Loan*((1+(r/n))**(i)) - payment*(((1 + r/n)**(i)-1)/(r/n))*(1+r/n)

    #amount of interest payed on i month
    int <- PP*(r/n)

    x = x + payment #this adds up total spent on loan
    tot_int = tot_int + int #total interest paid
    
    cf <- data.frame(i,PP,int,x,tot_int)    
    
    names(cf) <- c("Month","Loan","Interest","Total Spent","Total Interest")
    df <- rbind(df,cf)
    i = i + 1
    
    if (i==180) {
      break
    }
    
  }
  
  x <- df[nrow(df),c("Total Spent")]
  tot_int <- df[nrow(df),c("Total Interest")]
  
  return(c(tot_int,x,i))
  
}

d <- data.frame(Payment = numeric(),Total_Interest = numeric(),Total_Spent = numeric(),Total_Months = numeric())

old <- Sys.time()
for(i in seq(1, 2000,1)){
  
  x <- StudentLoan_Func(i,30499.29) #how much loan is
  cf <- data.frame(i,x[1],x[2],x[3]) 
  
  names(cf) <- c("Payment","Total_Interest","Total_Spent","Total_Months")
  d <- rbind(d,cf)
}
new <- Sys.time() - old # calculate difference
print(new)


plot_ly(d, x = ~Payment,y = ~Total_Interest,type = 'scatter', mode = 'lines',line = list(color = '#4daf4a', width = 2))

plot_ly(d, x = ~Payment,y = ~Total_Months,type = 'scatter', mode = 'lines',line = list(color = '#377eb8', width = 2))
