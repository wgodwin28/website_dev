library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(scales)
library(kableExtra)
library(DT)
library(mosaic)
library(googlesheets)
library(fetch)
library(shinydashboard)

#pull in advertiser labels data
ad_party <- fetch::fetchGoogle("https://docs.google.com/spreadsheets/d/e/2PACX-1vS6fE8z9WFZPDqpSQCVA3AA9AI5vjYf1LF6VXYCn6LiW3-AjqFojVcravtGbCwbUplaue48Dle-IAug/pub?gid=1509796885&single=true&output=csv")
ad_party <- ad_party %>%
  select(Advertiser_ID, party_cat) %>%
  mutate(Party = case_when(
    party_cat == 1 ~ "Democrat",
    party_cat == 2 ~ "Republican",
    party_cat == 3 | party_cat == 4 ~ "Undetermined"
  )) %>%
  filter(!is.na(Party))

#run script that updates and cleans data
#setwd("~/Desktop/website_dev/google_ads/code/shiny/")
#source("../clean_app.R")
data <- read.csv("shiny_data.csv")

data <- left_join(data, ad_party, by="Advertiser_ID")

#set date parameters
startDate <- data %>% 
  summarise(date=format(min(as.Date(week_start)), "%B %Y")) %>% 
  pull()
endDate <- data %>% 
  summarise(date=format(max(as.Date(week_start)), "%B %Y")) %>% 
  pull()
downDate <- data %>% 
  summarise(date=format(max(as.Date(week_start)), "%b %d %Y")) %>% 
  pull()

#maximum number of days any ad ran
max_day <- max(data$Num_of_Days)

#user interface
ui <- fluidPage(
  titlePanel(paste0("Political Advertising on Google Platforms - ", startDate, " to ", endDate)),
  sidebarLayout(
    sidebarPanel(paste0("Last data download: ", downDate),
     checkboxGroupInput(inputId = "partyInput", label = "Political Leaning",
                        choices = c("Democrat", "Republican", "Undetermined"),
                        selected = c("Democrat", "Republican")),
     helpText("Advertiser political leaning was assessed based on 2-person independent web search. Cases where agreement wasn't reached were labeled 'Undetermined'."),
      sliderInput(inputId = "daysInput", label = "Number of days aired", value = c(1, max_day), min = 1, max=max_day, step=5),
      checkboxGroupInput(inputId = "ad_typeInput", label = "Ad Type",
                   choices = c("Text", "Video", "Image"),
                   selected = c("Text", "Video", "Image")),
      selectInput(inputId = "price_catInput", label = "Price Category (USD)",
                  choices = c("0-100", "100-1k", "1k-50k", "50k-100k", "100k +")),
     checkboxInput(inputId = "transInput", label = "Plot Y-axis on square root scale",
                   value = F),
     # adding the new div tag to the sidebar            
     tags$div(class="header", checked=NA,
              #tags$p("Interested in the underlying code?"),
              tags$a(href="https://github.com/wgodwin28/website_dev/blob/master/google_ads/code/shiny/app.R", "Find code here"))
    ),
    mainPanel(
      plotOutput("coolplot"), #output placeholder 1
      br(), br(),
      dataTableOutput("topTen") #output placeholder 2
    )
  )
)

#back-end server
server <- function(input, output) {
  #generate data that is reactive to app inputs
  #filtered <- reactive({
  #})
  
  ##generate time series plot
  output$coolplot <- renderPlot({
    #filter to appropriate data
    filtered <- data %>%
      filter(pricing_cat == input$price_catInput,
             Party %in% input$partyInput,
             Ad_Type %in% input$ad_typeInput,
             Num_of_Days >= input$daysInput[1],
             Num_of_Days <= input$daysInput[2]) %>%
      group_by(Party=factor(Party, levels = c("Democrat", "Republican", "Undetermined")), 
               date=as.Date(week_start), 
               Impressions=factor(Impressions, levels = c("Under 10k", "10k-100k", "100k-1M", "1M-10M", "10M+"))) %>%
      summarize(weekly_ad_count=n())
    
    #set y-axis limits
    y.min <- 0
    y.max <- max(filtered$weekly_ad_count)
    
    #set specified transformation
    trans <- ifelse(input$transInput, "sqrt", "identity")
    
    #plot
    ggplot(filtered, aes(date, weekly_ad_count, color=Party, size=Impressions)) +
      geom_point(alpha=0.4) +
      xlab("Ad Start Date") +
      ylab("Weekly Ad Total") +
      ylim(y.min, y.max) +
      scale_color_manual(values = c("blue", "red", "green")) +
      geom_vline(xintercept = as.Date("2018/11/06"), linetype=4) +
      annotate("text", x=as.Date("2018/10/31"), y=(0.9*y.max), label="Midterm", colour="black", angle=90, text=element_text(size=11)) +
      scale_y_continuous(breaks = pretty_breaks(), limits = c(y.min, y.max), trans = trans) +
      scale_x_date(labels = date_format("%b - %Y"), breaks = date_breaks("1 month")) +
      ggtitle("Total Advertisements by Week") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #generate top ten table
  output$topTen <- renderDataTable({
    table_data <- data %>%
      filter(pricing_cat == input$price_catInput,
             Party %in% input$partyInput,
             Ad_Type %in% input$ad_typeInput,
             Num_of_Days >= input$daysInput[1],
             Num_of_Days <= input$daysInput[2]) %>%
      group_by(Advertiser_Name) %>%
      summarize(total_ads=n()) %>%
      mutate("Percentage" = percent(total_ads/sum(total_ads))) %>%
      left_join(data %>% distinct(Advertiser_Name, Party), by="Advertiser_Name") %>%
      arrange(-total_ads) %>%
      rename("Total Ads" = total_ads, Advertiser = Advertiser_Name)
    datatable(table_data)
  })
}

shinyApp(ui = ui, server = server)

#deployApp(appName = "shiny_google_political_ads", appDir="~/Desktop/website_dev/google_ads/code/shiny/")

# data %>%
#   filter(pricing_cat == "0-100") %>%
#   group_by(Advertiser_Name) %>%
#   summarize(total_ads=n()) %>%
#   mutate("Percentage" = percent(total_ads/sum(total_ads))) %>%
#   arrange(-total_ads) %>%
#   left_join(data %>% distinct(Advertiser_Name, Party), by="Advertiser_Name")
# 
