rm(list = ls())
library(ggplot2)
###This calculates the amount over time of a compounding interest investment or loan. The contribution takes place at the beginning of the month.The payments made towards the loan vs the contributions to retirement are out of $2000/month. (e.g. if you make a loan payment of 1750, the retirement contribution is 250)

#A = amount after n years
#P = principal
#c = contribution
#r = annual int rate
#n = number of times interest is compounded per year
#t = number of months

Retirement_Func = function(y){
  
  ###initializations
  t = 12*30
  Months <- c(1:t)
  Saved <- matrix(ncol=1,nrow=t)
  Loans <- matrix(ncol=1,nrow=t)
  payment = y
  c = 2000-payment
  x = 0
  
  ###loans loop over time
  for(i in 1:t){
    
    P = 237500 #loan
    r = .0433 #loan interest rate
    n = 12
    
    #amount after i months
    A <- P*((1+(r/n))**(i)) - payment*(((1 + r/n)**(i)-1)/(r/n))*(1+r/n)
    
    #amount after i - 1 months
    Ab<- P*((1+(r/n))**(i-1)) - payment*(((1 + r/n)**(i-1)-1)/(r/n))*(1+r/n)
    
    if (A <= 0){
      Loans[i,] <- 0
    }
    else{
      Loans[i,] <- A 
    }  
    #once loans hit zero add loan payment to investments
    if (A<0 && Ab>0){
      offset<-i
    }
    ###savings loop over time
    
    PP = 0    #amount initially saved
    rr = .06  #rate of return on saved (assuming its invested)
    
    if (A < 0){
      #once loans hit zero add loan payment to investments
      AA <- PP*((1+(rr/n))**(i)) + (c)*(((1 + rr/n)**(i)-1)/(rr/n))*(1+rr/n) + (payment)*(((1 + rr/n)**(i-offset)-1)/(rr/n))*(1+rr/n)
      Saved[i,] <- AA
    }
    else{
      AA <- PP*((1+(rr/n))**(i)) + c*(((1 + rr/n)**(i)-1)/(rr/n))*(1+rr/n)
      Saved[i,] <- AA 
      
      x = x + payment #this adds up total spent on loan
    } 
    
  }
  
  ###create dataframes
  data1 <- data.frame(Months,Saved,Loans)
  
  #data1$col3 <- data$col1 + data$col2
  
  ###plot and view table
  #plot(data,pch=20)
  #ggplot(data1,aes(Months)) + geom_line(aes(y = Loans)) + coord_cartesian(ylim = (0:50000),xlim = (0:10) )
  #ggplot(data1,aes(Months)) + geom_line(aes(y = Saved))
  ggplot(data1,aes(Months)) + geom_line(aes(y = Saved,color = 'Saved')) + geom_line(aes(y = Loans,color='Loans'))
  Saved[t]-x
  return(Saved[t]-x)
}


Payment <- c(1:2000)
Trend <- matrix(ncol=1,nrow=2000)

for(i in 750:2000){
  x <- Retirement_Func(i)
  Trend[i,] <-x
}
data2 <- data.frame(Payment,Trend)
ggplot(data2,aes(Payment)) + geom_line(aes(y = Trend,color = 'trend'))  + coord_cartesian(xlim = (750:2000) )
