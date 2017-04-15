library(shiny)
library(ggvis)
library(dplyr)
library(tidyr)
library(tibble)
library(nzelect)

load("sims.rda")
load("parties.rda")

n <- 5000 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  parties2 <- reactive({
    gsub("M.ori", "Maori", input$coal_members)
  })
  
  #---------------simulate seats----------------
  m_votes_2014 <- data_frame(
    labour = c(9712, 7533, 8089, 9753, 8445, 12191, 5837),
    mana_maori = c(11548, 8695, 8475, 8928, 6887, 7612, 15208)
  ) %>%
    mutate(
      mana_maori = mana_maori * 0.9,
      prob_lab = labour / (mana_maori + labour),
      prob_oth = 1 - prob_lab)
  
  
  # a filler data frame of the three parties that don't get any simulated electorate seats.
  # Note that NZ First isn't really a certainty to get their seat, but it doesn't matter as they are 
  # almost certainly above the 5% threshold anyway, so we don't bother to simulate Northland
  filler <- data.frame(
    party = c("Conservative", "Green", "NZ First"),
    seats = c(0, 0, 1),
    sim = rep(1:n, each = 3)
  )
  
  # see https://en.wikipedia.org/wiki/M%C4%81ori_electorates for the true names of the Maori electorates.
  # For convenience I call them m1 : m7.
  # Also note - the procedure below often gives zero electorate seats to National.  This doesn't
  # impact on seat allocation as they always exceed the 5% party vote threshhold; they only
  # need to be in the simulation at all for the off chance they take Epsom off ACT.
  electorate_sims <- data_frame(
    orahiu = sample(c("United Future", "Labour"), prob = c(0.6, 0.4), size = n, replace = TRUE),
    epsom = sample(c("ACT", "National", "Labour"), prob = c(0.8, 0.1, 0.1), size = n, replace = TRUE),
    m1 = sample(c("Labour", "Mana"), prob = m_votes_2014[1, 3:4], size = n, replace = TRUE),
    m2 = sample(c("Labour", "Maori"), prob = m_votes_2014[2, 3:4], size = n, replace = TRUE),
    m3 = sample(c("Labour", "Maori"), prob = m_votes_2014[3, 3:4], size = n, replace = TRUE),
    m4 = sample(c("Labour", "Maori"), prob = m_votes_2014[4, 3:4], size = n, replace = TRUE),
    m5 = sample(c("Labour", "Maori"), prob = m_votes_2014[5, 3:4], size = n, replace = TRUE),
    m6 = sample(c("Labour", "Maori"), prob = m_votes_2014[6, 3:4], size = n, replace = TRUE),
    m7 = sample(c("Labour", "Maori"), prob = m_votes_2014[7, 3:4], size = n, replace = TRUE)
  ) %>%
    mutate(sim = 1:n()) %>%
    gather(seat, party, -sim) %>%
    group_by(party, sim) %>%
    summarise(seats = n()) %>%
    ungroup() %>%
    rbind(filler) %>%
    spread(party, seats, fill = 0) 
  
  seats <- t(sapply(1:n, function(i){
    allocate_seats(votes = as.numeric(sims[i, ]), 
                   electorate = as.numeric(electorate_sims[i, -1]),
                   parties = parties)$seats_v
  })) %>%
    as_tibble()
  
  
  
  seats_long <- seats %>%
    mutate(sim = 1:n()) %>%
    gather(party, seats, -sim) 
  
  #--------------build coalitions-------------
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
