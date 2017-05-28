library(shiny)
library(ggvis)
library(dplyr)
library(tidyr)
library(tibble)
library(nzelect)

load("sims.rda") # simulated party vote
load("parties.rda")
load("parties_ordered.rda")

n <- 2000 
sims <- sims[1:n, ]

# a filler data frame of the three parties that don't get any simulated electorate seats.
# Note that NZ First isn't really a certainty to get their seat, but it doesn't matter as they are 
# almost certainly above the 5% threshold anyway, so we don't bother to simulate Northland
filler <- data.frame(
  party = c("Conservative", "Green", "NZ First"),
  seats = c(0, 0, 1),
  sim = rep(1:n, each = 3)
)

shinyServer(function(input, output) {
  # version of parties chosen by user without macrons:
  parties2 <- reactive({
    tmp <- gsub("M.ori", "Maori", input$coal_members)
    return(tmp)
  })
  
  #---------------simulate seats----------------
  # Turn the Maori seat chances chosen by user into a data frame:
  maori_probs <- reactive({
    tmp <- data.frame(labour = c(input$m1, input$m2, input$m3, 
                                 input$m4, input$m5, input$m6, input$m7)) %>%
      mutate(other = 1 - labour)
    return(tmp)
  })

  # Allocate individual electorate seats for each row of the simulation:  
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
  
  # Allocate seats on basis of party vote and electorates
  seats <- reactive({t(sapply(1:n, function(i){
    allocate_seats(votes = as.numeric(sims[i, ]), 
                   electorate = as.numeric(electorate_sims()[i, -1]),
                   parties = gsub("M.ori", "Maori", parties))$seats_v
  })) %>%
    as_tibble()
  })
  
  # long tidy version of the reactive (user-modified) seats data frame:
  seats_long <- reactive({
    seats() %>%
    mutate(sim = 1:n()) %>%
    gather(party, seats, -sim) 
  })
  
  #--------------build coalitions-------------
  # seats and percent of seats for user-chosen coaltion:
  the_seats <- reactive({
    # we want the colour of the biggest party mentioned
    # in the user-chosen party list:
    tmp_col <- parties_ordered %>%
      filter(party %in% parties2()) 
    
    # sum up the seats (and the percent of seats - depends on
    # how many seats in parliament) for the given coalition,
    # and add on the biggest party colour as part of the data frame:
    tmp_df <- seats_long() %>%
    group_by(sim) %>%
    summarise(coal_seats = sum(seats[ party %in% parties2()]),
              coal_perc = coal_seats / sum(seats)) %>%
      ungroup() %>%
      mutate(Colour = tmp_col[1, "Colour"])
  })

  # chance of winning (needs to be based on percent):
  chance <- reactive({
    tmp <- as.vector(mean(the_seats()$coal_perc > 0.5))
    tmp <- paste("<p>Chance of coalition of<i>", paste(input$coal_members, collapse =" + "), 
                 "</i> winning >50% of seats is<b>", format(round(tmp, 2), nsmall = 2), "</b></p>")
    return(tmp)
  })
  output$prob <- renderText({chance()})
  
  #==============graphics x 2========================
    the_seats %>%
    ggvis(~coal_perc) %>%
      layer_densities(fill := ~Colour, stroke:= ~Colour) %>%
      set_options(height = 275) %>%
      add_axis("x", title ="Percentage of total seats", format = "%") %>%
      bind_shiny("perc_plot")
  
  tool_func <- function(x){
    seat_bar <- (x[[2]] + x[[3]]) / 2
    prob_bar <-  round(x[[4]] / n * 100, 1)
    paste0("<p>Probability of ", seat_bar, " seats is ", prob_bar, "%</p>")
  }
  
    the_seats %>%
      ggvis(~coal_seats) %>%
      layer_histograms(fill := ~Colour, stroke:= "white", width = 1) %>%
      set_options(height = 275) %>%
      add_axis("x", title = "Number of seats") %>%
      add_axis("y", title = paste("Number of simulations out of", format(n, big.mark = ","))) %>%
      add_tooltip(html = tool_func) %>%
      bind_shiny("seats_plot")
    
    observeEvent(input$reset_input, {
      shinyjs::reset("parameters")
    })      
  

})
