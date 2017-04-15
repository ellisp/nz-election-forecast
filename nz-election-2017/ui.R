library(shiny)
library(ggvis)

load("parties.rda")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Forecasts for New Zealand General Election 2017"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(3,
       checkboxGroupInput("coal_members",
                   "Select coalition members",
                   choices = parties,
                   selected = c("ACT", "National", "United Future", "Māori")
         
       ),
       hr(),
       sliderInput("epsom", "Epsom - probability of ACT win", min = 0, max = 1, value = 0.6),
       sliderInput("ohariu", "Ohariu - probability of United Future win", min = 0, max = 1, value = 0.6)
       ),
    column(3, 
       sliderInput("m1", "Ikaroa-Rawhiti - probability of Labour win", min = 0, max = 1, value = 0.6),
       sliderInput("m2", "Tainui - probability of Labour win", min = 0, max = 1, value = 0.6),
       sliderInput("m3", "Tamaki Makaurau - probability of Labour win", min = 0, max = 1, value = 0.6),
       sliderInput("m4", "Te Tai Hauauru - probability of Labour win", min = 0, max = 1, value = 0.6),
       sliderInput("m5", "Te Tai Tokerau - probability of Labour win", min = 0, max = 1, value = 0.6),
       sliderInput("m6", "Te Tai Tonga - probability of Labour win", min = 0, max = 1, value = 0.6),
       sliderInput("m7", "Wairaki - probability of Labour win", min = 0, max = 1, value = 0.6)
    ),
    column(6,
           
           ggvisOutput("perc_plot"),
           ggvisOutput("seats_plot")
           )
    )
    
    
  )
)
