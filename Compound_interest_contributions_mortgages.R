rm(list = ls())
library(ggplot2)
source("multiplot.R")
###This calculates the amount over time of a compounding interest investment or loan. The contribution takes place at the beginning of the month.The payments made towards the loan vs the contributions to retirement are out of $1000/month. (e.g. if you make a loan payment of 750, the retirement contribution is 250)

#A = amount after n years
#P = principal
#c = contribution
#r = annual int rate
#n = number of times interest is compounded per year
#t = number of months


###initializations
t = 12*30
#Months <- c(1:t)

payment = 1300.8258
c = 2000-payment
x = 0
tot_int = 0
House_Price = 270000
P = House_Price-(House_Price*.05) #loan (down payment = 5%)

df <- data.frame(0,0,P,0)
names(df) <- c("Month","Saved","Loan","Interest")

###loans loop over time
for(i in 1:t){

  r = .0425 #loan interest rate
  n = 12
  
  #amount after i months
  A <- P*((1+(r/n))**(i)) - payment*(((1 + r/n)**(i)-1)/(r/n))*(1+r/n)
  #amount after i - 1 months
  Ab <- P*((1+(r/n))**(i-1)) - payment*(((1 + r/n)**(i-1)-1)/(r/n))*(1+r/n)
  
  #amount of interest payed on i month
  int <- A*(r/n)
  
  #once loans hit zero add loan payment to investments
  if (A<0 && Ab>0){
    offset<-i
  }
  ###savings loop over time
  PP = 0
  rr = .05
  
  if (A <= 0){
    #once loans hit zero add loan payment to investments
    AA <- PP*((1+(rr/n))**(i)) + (c)*(((1 + rr/n)**(i)-1)/(rr/n))*(1+rr/n) + (payment)*(((1 + rr/n)**(i-offset)-1)/(rr/n))*(1+rr/n)
    A = 0
    int = 0
    cf <- data.frame(i,AA,A,int)
  }
  else{
    AA <- PP*((1+(rr/n))**(i)) + c*(((1 + rr/n)**(i)-1)/(rr/n))*(1+rr/n)
    cf <- data.frame(i,AA,A,int)
    x = x + payment #this adds up total spent on loan
    tot_int = tot_int + int #total interest paid
  } 
  
  names(cf) <- c("Month","Saved","Loan","Interest")
  df <- rbind(df,cf)
}

###plot and view table
p1<-ggplot(df,aes(x = Month)) + 
  geom_line(aes(y = Saved,color = 'Saved')) + 
  geom_line(aes(y = Loan,color='Loan'))
#p2<-ggplot(df,aes(x = Month)) +
#  geom_line(aes(y = Interest,color='Interest'))
p1
#p2
tot_int
x
cf
#multiplot(p1,p2)
