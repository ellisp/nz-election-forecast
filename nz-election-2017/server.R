library(shiny)
library(ggvis)
library(dplyr)
library(tidyr)
library(tibble)
library(nzelect)

load("sims.rda")
load("parties.rda")

n <- 5000 


# a filler data frame of the three parties that don't get any simulated electorate seats.
# Note that NZ First isn't really a certainty to get their seat, but it doesn't matter as they are 
# almost certainly above the 5% threshold anyway, so we don't bother to simulate Northland
filler <- data.frame(
  party = c("Conservative", "Green", "NZ First"),
  seats = c(0, 0, 1),
  sim = rep(1:n, each = 3)
)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  parties2 <- reactive({
    tmp <- gsub("M.ori", "Maori", input$coal_members)
    return(tmp)
  })
  
  #---------------simulate seats----------------
  maori_probs <- reactive({
    tmp <- data.frame(labour = c(input$m1, input$m2, input$m3, 
                                 input$m4, input$m5, input$m6, input$m7)) %>%
      mutate(other = 1 - labour)
    print(tmp)
    return(tmp)
  })

  
  # see https://en.wikipedia.org/wiki/M%C4%81ori_electorates for the true names of the Maori electorates.
  # For convenience I call them m1 : m7.
  # Also note - the procedure below often gives zero electorate seats to National.  This doesn't
  # impact on seat allocation as they always exceed the 5% party vote threshhold; they only
  # need to be in the simulation at all for the off chance they take Epsom off ACT.
  electorate_sims <- reactive({data_frame(
    orahiu = sample(c("United Future", "Labour"), prob = c(input$ohariu, 1-input$ohariu), size = n, replace = TRUE),
    epsom = sample(c("ACT", "National"), prob = c(input$epsom, 1-input$epsom), size = n, replace = TRUE),
    m1 = sample(c("Labour", "Maori"), prob = maori_probs()[1, 1:2], size = n, replace = TRUE),
    m2 = sample(c("Labour", "Maori"), prob = maori_probs()[2, 1:2], size = n, replace = TRUE),
    m3 = sample(c("Labour", "Maori"), prob = maori_probs()[3, 1:2], size = n, replace = TRUE),
    m4 = sample(c("Labour", "Maori"), prob = maori_probs()[4, 1:2], size = n, replace = TRUE),
    m5 = sample(c("Labour", "Mana"),  prob = maori_probs()[5, 1:2], size = n, replace = TRUE),
    m6 = sample(c("Labour", "Maori"), prob = maori_probs()[6, 1:2], size = n, replace = TRUE),
    m7 = sample(c("Labour", "Maori"), prob = maori_probs()[7, 1:2], size = n, replace = TRUE)
  ) %>%
    mutate(sim = 1:n()) %>%
    gather(seat, party, -sim) %>%
    group_by(party, sim) %>%
    summarise(seats = n()) %>%
    ungroup() %>%
    rbind(filler) %>%
    spread(party, seats, fill = 0)
  })
  
  seats <- reactive({t(sapply(1:n, function(i){
    allocate_seats(votes = as.numeric(sims[i, ]), 
                   electorate = as.numeric(electorate_sims()[i, -1]),
                   parties = gsub("M.ori", "Maori", parties))$seats_v
  })) %>%
    as_tibble()
  })
  
  
  seats_long <- reactive({
    seats() %>%
    mutate(sim = 1:n()) %>%
    gather(party, seats, -sim) 
  })
  
  #--------------build coalitions-------------
  the_seats <- reactive({
    seats_long() %>%
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
