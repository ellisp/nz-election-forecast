library(shiny)
library(ggvis)

load("parties.rda")

minsl <- 0.05
maxsl <- 0.95

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
       HTML("<p>The choices of parties for a coalition above lead to predictions of the number of 
seats in Parliament.   Party vote is simulated based on the model described at 
<a href='http://ellisp.github.io/elections/elections.html'>Peter's Stats Stuff</a>.
Conversion of party vote to number of seats requires assumptions of some key electorate outcomes.
Adjusting the sliders for the electorates below and to the right will create new simulations of
electorate seats, and hence of total seats allocated.</p>"),
       
       hr(),
       sliderInput("epsom", "Epsom - probability of ACT win", min = minsl, max = maxsl, value = 0.8),
       sliderInput("ohariu", "Ohariu - probability of United Future win", min = minsl, max = maxsl, value = 0.6)
       ),
    column(3, 
       sliderInput("m1", "Ikaroa-Rawhiti - probability of Labour win", min = minsl, max = maxsl, value = 0.48),
       sliderInput("m2", "Tainui - probability of Labour win", min = minsl, max = maxsl, value = 0.49),
       sliderInput("m3", "Tamaki Makaurau - probability of Labour win", min = minsl, max = maxsl, value = 0.51),
       sliderInput("m4", "Te Tai Hauauru - probability of Labour win", min = minsl, max = maxsl, value = 0.55),
       sliderInput("m5", "Te Tai Tokerau - probability of Labour win", min = minsl, max = maxsl, value = 0.58),
       sliderInput("m6", "Te Tai Tonga - probability of Labour win", min = minsl, max = maxsl, value = 0.64),
       sliderInput("m7", "Wairaki - probability of Labour win", min = minsl, max = maxsl, value = 0.30)
    ),
    column(6,
           
           ggvisOutput("perc_plot"),
           htmlOutput("prob"),
           ggvisOutput("seats_plot")
           )
    )
    
    
  )
)
