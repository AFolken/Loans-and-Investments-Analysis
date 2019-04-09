#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotly)
library(shiny)


StudentLoan_Payment_Func = function(interest,Loan){
  
  d <- data.frame(Payment = numeric(),Total_Interest = numeric(),Total_Spent = numeric(),Total_Months = numeric())
  
  r = interest                #loan interest rate
  n = 12                      #how interest rate is divided
  ymax = ceiling((r/n)*Loan)  #this was needed to avoid a negative value in the log when calculating total payments (essentially, payments lower than this amount will result an infinite number of payments because you aren't paying more than the interest)
  
  for(i in seq(ymax, 2000,1)){  #loops through a spectrum of payment amounts starting at ymax (see above def.) through 2000 
    
    tp <- (-log(1-((r/n)*Loan/i)))/(log(1+(r/n)))             # calculates total payments (le googel)
    
    Total_Spent <- tp*i                                       #total payments * payment amt = total spent
    Total_Interest <- Total_Spent-Loan                        #total interest would be the total spent minus the original loan balance
    Total_years <- tp/12
    
    cf <- data.frame(i,round(Total_Interest,digits=2),round(Total_Spent,digits=2),round(tp,digits=2),round(Total_years,digits=2) )
    names(cf) <- c("Payment","Total_Interest","Total_Spent","Total_Months","Total_Years")
    
    d <- rbind(d,cf)
  }
  return (d)
  
}

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Loan Payment Spectrum"),
   
   # Sidebar with a slider input for number of bins 
   sidebarPanel(
     numericInput('interest', 'Interest Rate (0.08 = 8%)', 0.08,
                  min = 0, max = 1, step = .001),
     numericInput('loan', 'Loan Amount ($)', 30000,
                  min = 1, step = 500),
     submitButton("Submit"),
     "Enter the interest rate and loan amount above. The two plots and table on the right will show you how much it will cost and how long it will take to pay off the loan depending on how much you pay monthly.
     
       The total interest is the amount you are paying in addition to paying back the loan.
     
       The total months is the total time it will take to pay off the loan.
     
       The plots are interactive. Zoom in/out and hover to analyze the data."
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
        tabPanel("Total Interest",
         plotlyOutput("plot"),height = "90%",padding = c(0,'50%')
        ),
        tabPanel("Total Months",
                 plotlyOutput("plot2"),height = "90%",padding = c(0,'50%')
        ),
        tabPanel("Table", DT::dataTableOutput("table"))
      )
      )
)

# Define server logic
server <- function(input, output) {
  
  d <- reactive({StudentLoan_Payment_Func(input$interest,input$loan)})
  
  output$plot <- renderPlotly({
    plot_ly(d(), x = ~Payment,y = ~Total_Interest,type = 'scatter', mode = 'lines',line = list(color = '#4daf4a', width = 2),height = 600) %>%
      layout(
        hovermode = 'compare',
        title = "<br>Total Interest by Payment",
        xaxis = list(range = c(0,2050),
                     title = 'Payment'),      
        yaxis = list(range = c(0,50000),
                     title = 'Total Interest')      #total interest range set to $50000 (if you're planning on spending more you need to reanalyze your life choices)
        
      )
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(d(), x = ~Payment,y = ~Total_Months,type = 'scatter', mode = 'lines',line = list(color = '#377eb8', width = 2),height = 600) %>%
      layout(
        hovermode = 'compare',
        title = "<br>Total Months by Payment",
        xaxis = list(range = c(0,2050),
                     title = 'Payment'),      
        yaxis = list(range = c(0,370),
                     title = 'Total Months')             #total months range set to 30 years (if you're planning on taking longer :'( )
      )
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(d(), options = list(pageLength = 15))
    )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

