## app.R ##
library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)

# read clean data
#setwd("seawall/code/shiny/")
df <- read_csv("app_data.csv")
xvars <- c("Poverty", "Unemployed_LF", "Not_in_LF", "Transportation", "Construction",
           "Agriculture_fishing", "Construction_all", "Manufacturing_all", 
           "Median_HH_income", "Health_insurance_cov", "Health_insurance_priv")


#user interface
ui <- 
  pageWithSidebar(
    headerPanel(paste0("The Tide is High")),
    sidebarPanel(
      selectInput(inputId = "xvarInput", 
                  label = "Variable", 
                  choices = xvars),
      checkboxGroupInput(inputId = "smoother", 
                         label = "Smoother", 
                         choices = c("loess", "lm", "glm"), 
                         selected = "loess")
    ),
    mainPanel(
      plotOutput("scatter") #output placeholder 1
    )
  )

#back-end server
server <- function(input, output) {
  #generate data that is reactive to app inputs
  #filtered <- reactive({
  #})
  
  ##generate time series plot
  output$scatter <- renderPlot({
    #filter to appropriate data
    p <- ggplot(df, aes_string(input$xvarInput, "seawall_cost_percap")) +
      geom_point(aes(color=Region, text=paste0("State: ", state))) +
      geom_smooth(method = "lm", color="black", se=F) +
      geom_smooth(se=F) +
      ylab("Seawall Cost per capita") +
      theme_bw()
    p
    #print(ggplotly(p))
  })
}
shinyApp(ui = ui, server = server)
# 
# 
# pageWithSidebar(
#   
#   headerPanel("Diamonds Explorer"),
#   
#   sidebarPanel(
#     
#     sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
#                 value=min(1000, nrow(dataset)), step=500, round=0),
#     
#     selectInput('x', 'X', names(dataset)),
#     selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
#     selectInput('color', 'Color', c('None', names(dataset))),
#     
#     checkboxInput('jitter', 'Jitter'),
#     checkboxInput('smooth', 'Smooth'),
#     
#     selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
#     selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
#   ),
#   
#   mainPanel(
#     plotOutput('plot')
#   )
# )