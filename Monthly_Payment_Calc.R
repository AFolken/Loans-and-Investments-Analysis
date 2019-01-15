Monthly_Payment_Calc = function(L,n,i){
  #L #loan amount
  #n #months (30 years = 360)
  #i interest rate annual
  c = i/12 #interest rate monthly
  L = L
  Payment = L*(c*((1 + c)**n))/((1 + c)**n - 1)
  
  Total_Paid = round(Payment*n,digits = 2)
  p = Total_Paid/L
  
  df <- data.frame(L,Payment,Total_Paid,p)
  
  return (df)
}
