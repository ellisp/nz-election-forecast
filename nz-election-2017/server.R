library(shiny)
library(ggvis)
library(dplyr)
library(tidyr)

load("seats.rda")

seats_long <- seats[ , 1:9] %>%
  mutate(sim = 1:n()) %>%
  gather(party, seats, -sim) %>%
  mutate(party = gsub("_", " ", party))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  the_seats <- reactive({
    seats_long %>%
    group_by(sim) %>%
    summarise(coal_seats = sum(seats[ party %in% gsub("M.ori", "Maori", input$coal_members)]),
              coal_perc = coal_seats / sum(seats))
  })
    
    the_seats %>%
    ggvis(~coal_perc) %>%
      layer_densities() %>%
      bind_shiny("perc_plot")
    
    the_seats %>%
      ggvis(~coal_seats) %>%
      layer_histograms() %>%
      bind_shiny("seats_plot")
    
      
  

})
