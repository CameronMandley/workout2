#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
future_value= function(amount, rate, years) {
  amount = amount*((1+rate/100)^years)
  return(amount)
}

annuity= function(contrib, rate, years) {
  amount = contrib*(((1+rate/100)^years - 1)/(rate/100))
  return(amount)
}

growing_annuity = function(contrib, rate, growth, years)  {
  amount = contrib*(((1 + rate/100)^years - (1+growth/100)^years)/(rate/100 - growth/100))
  return(amount)
}

ui <- fluidPage(
   
   # Application title
   titlePanel("Saving-Investing Modalities"),
   plotOutput("Timeline"),
   
   fluidRow(
     column(4, 
         sliderInput("init",
                     "Initial Amount",
                     min = 1,
                     max = 100000,
                     pre = "$",
                     sep = ",",
                     value = 1000),
         sliderInput("cont",
                     "Annual Contribution",
                     min = 0,
                     max = 50000,
                     pre = "$",
                     sep = ",",
                     value = 2000)
       ),
     column(4,
         sliderInput("rrate",
                     "Return Rate (in %)",
                     min = 0,
                     max = 20,
                     value = 5),
         sliderInput("grate",
                     "Growth Rate (in %)",
                     min = 0,
                     max = 20,
                     value = 2)
       ),
     column(4, 
         sliderInput("years",
                     "Years",
                     min = 0,
                     max = 50,
                     value = 10),
         selectInput("fac",
                          "Facet?",
                          c("No",
                            "Yes"))
         )
       ),
   mainPanel(
     tableOutput("summary_table")
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   d <- reactive({
      # generate bins based on input$bins from ui.R
     #Use melt function and create new data table
     mode1 = c()
     mode2 = c()
     mode3 = c()
     for (year in 0:input$years) {
       mode1 = append(mode1, future_value(amount = input$init, rate = input$rrate, years = year))
       mode2 = append(mode2, future_value(amount = input$init, rate = input$rrate, years = year) + annuity(contrib = input$cont, rate = input$rrate, years = year))
       mode3 = append(mode3, future_value(amount = input$init, rate = input$rrate, years = year) + growing_annuity(contrib = input$cont, rate = input$rrate, growth = input$grate, years = year))
     }
  
     modalities <- data.frame(
       modes = c(rep('mode1', length(mode1)), rep('mode2', length(mode2)), rep('mode3', length(mode3))),
       amounts = c(mode1, mode2, mode3)
     )
     library(dplyr)
     library(tidyr)
     modlong = gather(modalities, key="modes", value="amounts")
     return(modlong)
   })
     output$Timeline <- renderPlot({
       p = ggplot() +
         geom_line(data = d()[c(1:(input$years+1)), ], aes(x = c(0:input$years), y = d()$amounts[1:(input$years+1)], color = "no_contrib")) +
         geom_line(data = d()[c((input$years+2):(2*input$years+2)), ], aes(x = c(0:input$years), y = d()$amounts[(input$years+2):(2*input$years+2)], color = "fixed_contrib")) +
         geom_line(data = d()[c((2*input$years+3):(3*input$years+3)), ], aes(x = c(0:input$years), y = d()$amounts[(2*input$years+3):(3*input$years+3)], color = "growing_contrib")) +
         labs(title="Three Mode of Investing",
              x ="Years", y = "Balance")
       if (input$fac != 'No'){
         p <- p + facet_grid(modes ~ .)
       }
       
       p
     })
     output$summary_table <- renderTable({
       mode1 = c()
       mode2 = c()
       mode3 = c()
       for (year in 0:input$years) {
         mode1 = append(mode1, future_value(amount = input$init, rate = input$rrate, years = year))
         mode2 = append(mode2, future_value(amount = input$init, rate = input$rrate, years = year) + annuity(contrib = input$cont, rate = input$rrate, years = year))
         mode3 = append(mode3, future_value(amount = input$init, rate = input$rrate, years = year) + growing_annuity(contrib = input$cont, rate = input$rrate, growth = input$grate, years = year))
       }
       data.table(mode1, mode2, mode3)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

