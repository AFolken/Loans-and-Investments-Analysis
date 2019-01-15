rm(list = ls())

library(plotly)

StudentLoan_Payment_Func = function(interest,Loan){
  
  d <- data.frame(Payment = numeric(),Total_Interest = numeric(),Total_Spent = numeric(),Total_Months = numeric())
  
  r = interest #loan interest rate
  n = 12  #how interest rate is divided
  ymax = ceiling((r/n)*Loan) #this was needed to avoid a negative value in the log when calculating tp
  
  for(i in seq(ymax, 2000,1)){
    
    
    tp <- (-log(1-((r/n)*Loan/i)))/(log(1+(r/n))) # calculates total payments
    
    Total_Spent <- tp*i #total payments * payment amt = total spent
    Total_Interest <- Total_Spent-Loan
    
    cf <- data.frame(i,Total_Interest,Total_Spent,tp) 
    
    names(cf) <- c("Payment","Total_Interest","Total_Spent","Total_Months")
    d <- rbind(d,cf)
  }
  return (d)
  
}

d <- StudentLoan_Payment_Func(.0744,36115.98) # <---ENTER INTEREST AND LOAN AMOUNT HERE

#plots
plot_ly(d, x = ~Payment,y = ~Total_Interest,type = 'scatter', mode = 'lines',line = list(color = '#4daf4a', width = 2)) %>%
  layout(
    title = "",
    xaxis = list(range = c(0,2000)),
    yaxis = list(range = c(0,50000)) #total interest range set to $50000 (if you're planning on spending more you need to reanalyze your life choices)
  )

plot_ly(d, x = ~Payment,y = ~Total_Months,type = 'scatter', mode = 'lines',line = list(color = '#377eb8', width = 2))%>%
  layout(
    title = "",
    xaxis = list(range = c(0,2000)),
    yaxis = list(range = c(0,360)) #total months range set to 30 years (if you're planning on taking longer :( )
  )
