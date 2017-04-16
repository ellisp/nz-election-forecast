library(shiny)
library(shinyjs)
library(ggvis)

load("parties.rda")

minsl <- 0.05
maxsl <- 0.95

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),
  
  
  # Application title
  titlePanel("Forecasts for New Zealand General Election 2017"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    id = "parameters",
    column(3,
       checkboxGroupInput("coal_members",
                   "Select coalition members",
                   choices = parties,
                   selected = c("ACT", "National", "United Future", "Māori")
         
       ),
       #hr(),
       actionButton("reset_input", "Reset all assumptions"),
       hr(),
       
HTML("<p>The choices of parties for a coalition lead to predictions of the number of 
seats in Parliament.   Party vote is simulated based on the model described at 
<a href='http://ellisp.github.io/elections/elections.html'>Peter's Stats Stuff</a>.
Conversion of party vote to number of seats requires assumptions of some key electorate outcomes.
Adjusting the sliders for the electorates below and to the right will create new simulations of
electorate seats, and hence of total seats allocated.</p>"),
       
       
       hr(),
h4("Important electorates"),
       sliderInput("epsom", "Epsom - probability of ACT win", min = minsl, max = maxsl, value = 0.8),
       sliderInput("ohariu", "Ohariu - probability of United Future win", min = minsl, max = maxsl, value = 0.6),
p("Northland electorate, while of interest, is not included here as NZ First are believed likely to reach the 
5% threshold even if they do not retain the electorate.")
       ),
    column(3, hr(),
           h4("Maori electorates"),
       sliderInput("m1", "Ikaroa-Rawhiti - probability of Labour win", min = minsl, max = maxsl, value = 0.48),
       sliderInput("m2", "Tainui - probability of Labour win", min = minsl, max = maxsl, value = 0.49),
       sliderInput("m3", "Tamaki Makaurau - probability of Labour win", min = minsl, max = maxsl, value = 0.51),
       sliderInput("m4", "Te Tai Hauauru - probability of Labour win", min = minsl, max = maxsl, value = 0.55),
       sliderInput("m5", "Te Tai Tokerau - probability of Labour win", min = minsl, max = maxsl, value = 0.58),
       sliderInput("m6", "Te Tai Tonga - probability of Labour win", min = minsl, max = maxsl, value = 0.64),
       sliderInput("m7", "Wairaki - probability of Labour win", min = minsl, max = maxsl, value = 0.30),
       p("Te Tai Tokerau is assumed to go to either Mana or Labour; all other Maori electorates assumed to go to
either Maori or Labour.")
    ),
    column(6,
           h3("Probability distribution of won seats"),
           ggvisOutput("perc_plot"),
           htmlOutput("prob"),
           ggvisOutput("seats_plot"),
HTML("<p>All predictions should be taken with great caution.  See the <a href='http://ellisp.github.io/elections/elections.html'>
full description of the method</a> or the <a href = 'https://github.com/ellisp/nz-election-forecast'
sourec code</a> for more details.</p>")           
           )
    )
    
    
  )
)
