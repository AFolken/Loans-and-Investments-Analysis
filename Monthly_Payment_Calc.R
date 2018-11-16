###Monthly Payment Calculator
rm(list = ls())
library(ggplot2)

Monthly_Payment_Calc = function(L,n,i){
  #L #loan amount
  #n #months (30 years = 360)
  #i interest rate annual
  c = i/12 #interest rate monthly
  L = L - L*.05 #L*.05 is the down payment of 5%
  Payment = L*(c*((1 + c)**n))/((1 + c)**n - 1)
  
  Total_Paid = round(Payment*n,digits = 2)
  p = Total_Paid/L
  
  df <- data.frame(L,Payment,Total_Paid,p)
  
  return (df)
}

# use this for formatting
# options("scipen"=100, "digits"=6)

df <- data.frame("Interest"=double(),
                 "Payent"=double(),
                 "Total_Paid"=double(),
                 "Percent_of_Original_Loan"=double(),
                 stringsAsFactors=FALSE)
#names(df) <- c("Interest","Total Paid","Percent of Original Loan")

for(i in seq(200000,280000,5000)){
  nf <- Monthly_Payment_Calc(i,360,.0425)
  
  names(nf) <- c("Interest","Payment","Total_Paid","Percent_of_Original_Loan")
  df <- rbind(df,nf)
}

ggplot(df,aes(x = Interest)) + 
  geom_point(aes(y = Payment,color = 'Total')) #+
  #geom_point(aes(y = Percent_of_Original_Loan,color = 'Percent_of_Original_Loan'))

l = lm(formula = Interest ~ Payment,data = df)
summary(l)

