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
library(dplyr)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Savings Simulation"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4, sliderInput("initial",
                           "Initial Amount:",
                           min = 0,
                           max = 100000,
                           pre = "$",
                           step = 500,
                           value = 1000)
            ),
     column(4, sliderInput("return",
                           "Return Rate (in %)",
                           min = 0,
                           max = 20,
                           step = 0.1,
                           value = 5)
            ),
     column(4, sliderInput("years",
                           "Years",
                           min = 0,
                           max = 50,
                           step = 1,
                           value = 20)
            ),
     column(4, sliderInput("ann_contrib",
                           "Annual Contribution",
                           min = 0,
                           max = 50000,
                           step = 500,
                           pre = "$",
                           value = 2000)
            ),
     column(4, sliderInput("growth",
                           "Growth Rate (in %)",
                           min = 0,
                           max = 20,
                           step = 0.1,
                           value = 2)
            ),
     column(4, selectInput("facet",
                           "Facet?",
                           c("No", "Yes")
                           ))
     
         
      ),
      
      mainPanel(
        width = 12,
        hr(),
        h4("Timelines"),
        plotOutput("distPlot"),
        br(),
        h4("Balances"),
        verbatimTextOutput("view")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  modalities <- reactive({
    
    #' @title Future value
    #' @description calculates future value of an investment
    #' @param amount initial invested amount
    #' @param rate annual rate of return
    #' @param years number of years
    #' @return computed future value of an investment
    future_value <- function(amount, rate, years){
      fv <- amount*(1+rate)^years
      return(fv)
    }
    
    #' @title Future value of annuity
    #' @description calculates the future value of annuity
    #' @param contrib contributed amount
    #' @param rate annual rate of return
    #' @param years number of years
    #' @return computed future value of annuity
    annuity <- function(contrib, rate, years){
      fva <- contrib*(((1+rate)^years - 1)/rate)
      return(fva)
    }
    
    #' @title Future value of growing annuity
    #' @description calculates the future value of growing annuity
    #' @param contrib contributed amount
    #' @param rate annual rate of return
    #' @param growth annual growth rate
    #' @param years number of years
    #' @return computed future value of growing annuity
    growing_annuity <- function(contrib, rate, growth, years){
      fvga <- contrib*(((1+rate)^years - (1+growth)^years)/(rate-growth))
      return(fvga)
    }
    
    modalities <- matrix(nrow = 11, ncol = 4)
    modalities <- as.data.frame(modalities)
    
    for(i in 0:input$years){
      modalities[i+1, 1] <- i
      modalities[i+1, 2] <-  future_value(amount = input$initial, rate = input$return / 100, years = i)
      modalities[i+1, 3] <- future_value(amount = input$initial, rate = input$return / 100, years = i) + annuity(contrib = input$ann_contrib, rate = input$return / 100, years = i)
      modalities[i+1, 4] <- future_value(amount = input$initial, rate = input$return / 100, years = i) + growing_annuity(contrib = input$ann_contrib, rate = input$return / 100, growth = input$growth/100, years = i)
    }
    
    names(modalities) <- c("year", "no_contrib", "fixed_contrib", "growing_contrib")
    
    return(modalities)
  })
  
  plottype <- reactive({
    
    gather_modality <- gather(modalities(), key='variable', value='values', -year)
    
    gather_modality$variable <- factor(gather_modality$variable, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))
    
    if(input$facet == 'No'){
      plottype <- ggplot(gather_modality, aes_string('year', 'values', color = 'variable')) + geom_line() + 
        geom_point(size = 0.5) + labs(title = 'Three modes of investing') + 
        xlab('year') + ylab('value') 
    }
    else {
      plottype <- ggplot(gather_modality, aes(year, values, fill = variable)) + geom_line(aes(col = variable)) + 
        geom_area(alpha = 0.5) + geom_point(aes(col = variable), size = 0.5) + 
        facet_grid(.~ variable) + ylab("value") + labs(title = "Three modes of investing") + theme_bw()
    }
    return(plottype)
  })
  
   output$distPlot <- renderPlot({
      plottype()
   })
   
  output$view <- renderPrint({
    head(modalities(), n = input$years)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

