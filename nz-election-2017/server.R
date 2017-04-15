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
  parties2 <- reactive({
    gsub("M.ori", "Maori", input$coal_members)
  })
  
  the_seats <- reactive({
    seats_long %>%
    group_by(sim) %>%
    summarise(coal_seats = sum(seats[ party %in% parties2()]),
              coal_perc = coal_seats / sum(seats)) %>%
      ungroup()
  })
  
  chance <- reactive({
    tmp <- as.vector(mean(the_seats()$coal_perc > 0.5))
    tmp <- paste("<p>Chance of coalition of", paste(input$coal_members, collapse =", "), 
                 "winning is<b>", round(tmp, 2), "</b>.</p>")
    return(tmp)
  })
  
  output$prob <- renderText({chance()})
  
  
    the_seats %>%
    ggvis(~coal_perc) %>%
      layer_densities() %>%
      set_options(height = 275) %>%
      add_axis("x", title ="Percentage of total seats", format = "%") %>%
      bind_shiny("perc_plot")
    
    the_seats %>%
      ggvis(~coal_seats) %>%
      layer_histograms() %>%
      set_options(height = 275) %>%
      add_axis("x", title = "Number of seats") %>%
      add_axis("y", title = "Number of simulations out of 5,000") %>%
      bind_shiny("seats_plot")
    
      
  

})
