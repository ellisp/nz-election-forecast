#===============simulate electorates================

#' @param sims a matrix or data frame of simulated party vote, one row per simulation, one column per party
#' @param prefix either "ss", "gam" or "combined" for state-space or GAM model.  Used as prefix for the saved SVGs
simulate_seats <- function(sims, prefix, ThisElection, seed = 123){
  set.seed(seed)
  # parameter checks...
  
  
  
  n <- nrow(sims)
  all_parties <- sort(names(sims))
  sims <- sims[ , all_parties] # so columns in alphabetical order
  
  
  #-------------density of vote-------------
  sims_tidy <- sims %>%
    mutate(ID = 1:n()) %>%
    gather(Party, Vote, -ID) %>%
    group_by(ID) %>%
    mutate(Vote = Vote / sum(Vote))
  
  p1 <- sims_tidy %>%
          ggplot(aes(x = Vote, fill = Party)) +
          geom_density(alpha = 0.5, colour = NA) +
          facet_wrap(~Party, scales = "free") +
          scale_x_continuous(label = percent_format(accuracy = 1)) +
            scale_fill_manual(values = parties_v2) +
          theme(legend.position = "none") +
          labs(x = "Predicted party vote on election day", 
               y = "Likelihood",
               caption = "Source: http://freerangestats.info") +
          ggtitle(glue("Predicted party vote for the {format(as.Date(ThisElection), '%d %B %Y')} New Zealand General Election"),
                  "Simulations based on predictions from polling data")
  
  
  svg_png(p1, paste0("./output/", prefix, "-vote-predictions-density"), 9, 6)
  
  
  
  # According to this commentary https://www.waateanews.com/waateanews/x_news/MjQ1NzU/Opinion/What-latest-Poll-means-for-the-Maori-Party?
  # the Maori Party have a chance of winning two Maori electorates
  # For lack of any better way of doing it, I give Labour a probability of winning proportionate to their
  # votes in the last election compared to the combined Maori party votes, having knocked 20% off
  # those Maori party votes (this is the magic parameter "0.8" in the below)
  m_votes_last_election <- tibble(
    labour = c(14279, 15233, 12220, 13475, 14446, 13484, 14144),
    maori = c(2635, 3058, 2258, 3448, 1615, 2030, 4730)
  ) %>%
    mutate(
      maori = maori * 0.8,
      prob_lab = labour / (maori + labour),
      prob_oth = 1 - prob_lab)
  
  
  # a filler data frame of the parties that don't get any simulated electorate seats.
  # Note that NZ First isn't really a certainty to get their seat, but it doesn't matter as they are 
  # almost certainly above the 5% threshold anyway, so we don't bother to simulate Northland
  filler <- data.frame(
    party = c("Green", "NZ First"),
    seats = c(0, 0),
    sim = rep(1:n, each = 2)
  )
  
  # see https://en.wikipedia.org/wiki/M%C4%81ori_electorates for the true names of the Maori electorates.
  # For convenience I call them m1 : m7.
  # Also note - the procedure below often gives zero electorate seats to National.  This doesn't
  # impact on seat allocation as they always exceed the 5% party vote threshhold; they only
  # need to be in the simulation at all for the off chance they take Epsom off ACT.
  electorate_sims <- data_frame(
    epsom = sample(c("ACT", "National", "Labour"), prob = c(0.8, 0.1, 0.1), size = n, replace = TRUE),
    m1 = sample(c("Labour", "Maori"), prob = m_votes_last_election[1, 3:4], size = n, replace = TRUE),
    m2 = sample(c("Labour", "Maori"), prob = m_votes_last_election[2, 3:4], size = n, replace = TRUE),
    m3 = sample(c("Labour", "Maori"), prob = m_votes_last_election[3, 3:4], size = n, replace = TRUE),
    m4 = sample(c("Labour", "Maori"), prob = m_votes_last_election[4, 3:4], size = n, replace = TRUE),
    m5 = sample(c("Labour", "Maori"), prob = m_votes_last_election[5, 3:4], size = n, replace = TRUE),
    m6 = sample(c("Labour", "Maori"), prob = m_votes_last_election[6, 3:4], size = n, replace = TRUE),
    m7 = sample(c("Labour", "Maori"), prob = m_votes_last_election[7, 3:4], size = n, replace = TRUE)
  ) %>%
    mutate(sim = 1:n()) %>%
    gather(seat, party, -sim) %>%
    group_by(party, sim) %>%
    summarise(seats = n()) %>%
    ungroup() %>%
    rbind(filler) %>%
    spread(party, seats, fill = 0) 
  
  
  
  #============allocate seats==================
  # So far we have allocated "electorate" seats. Now we pad these out with
  # party lists in accordance iwth the MMP rules.
  
  seats <- t(sapply(1:n, function(i){
    allocate_seats(votes = as.numeric(sims[i, ]), 
                   electorate = as.numeric(electorate_sims[i, -1]),
                   parties = all_parties)$seats_v
  })) %>%
    as_tibble()
  
  names(seats) <- gsub("M.ori", "Maori", names(seats))
  names(seats) <- gsub("NZ First", "NZ_First", names(seats))
  
  seats <- seats %>%
    mutate(NatCoal = ACT + National + Maori,
           LabGreen = Labour + Green,
           LabGreenMaori = Labour + Green + Maori,
           LabGreenNZFirst = Labour + Green + NZ_First,
           NatCoalNZFirst = NatCoal + NZ_First)
  seats$Total <- apply(seats[ , 1:length(all_parties)], 1, sum)
  
  
  #==================presentation=====================
  
  p2 <- seats %>%
    select(National, Labour, NatCoal, LabGreen, LabGreenNZFirst, NatCoalNZFirst) %>%
    gather(Coalition, Seats) %>%
    mutate(lab_in = ifelse(grepl("^Lab", Coalition), "Labour-based", "Nationals-based")) %>%
    mutate(Coalition = gsub("^LabGreen", "Labour, Greens", Coalition),
           Coalition = gsub("^NatCoal", "National, ACT, Maori Party", Coalition),
           Coalition = gsub("NZFirst", ", NZ First", Coalition)) %>%
    ggplot(aes(x = Seats, fill = lab_in)) +
    facet_wrap(~Coalition, ncol = 1) +
    geom_histogram(alpha = 0.5, binwidth = 1, position = "identity", colour = NA)  +
    scale_y_continuous() +
    labs(title = glue("Possible government-formation in the {substring(ThisElection, 1, 4)} New Zealand election"),
         subtitle = glue("Likely seat counts for various combinations of parties, based on {nrow(sims)} simulations"),
         caption = paste("Source: http://freerangestats.info; model", prefix),
         y = "Likelihood") +
    expand_limits(y = c(0, 500)) +
    scale_fill_manual(values = as.character(c(parties_v2["Labour"], parties_v2["National"]))) +
    theme(legend.position = "none", panel.grid.minor = element_blank())
  
  svg_png(p2, paste0("./output/", prefix, "-results-density"), 9, 7)
  

  
  png(paste0("./output/", prefix, "-results-pairs.png"), 8 * 300, 7 * 300, res = 300)
  par(family = thefont, bty = "n", font.main = 1)
  seats %>%
    mutate(Other = as.ordered(ACT + Maori)) %>%
    dplyr::select(Green, Labour, National, NZ_First, Other) %>%
    pairs(diag.panel = panel.hist, upper.panel = panel.cor,
          main = paste("Possible outcomes for number of seats on", format(as.Date(ThisElection), "%d %B %Y")))
  dev.off()
  
  # space in `National ` is important as otherwise it gets confused with the original seat counts
  chances <- seats %>%
    summarise(`National ` = mean(National > Total / 2),
              `National needs a coalition similar to 2014` = mean(NatCoal > Total / 2 & National <= Total / 2),
              `Labour + Green win by themselves` = mean(LabGreen > (Total / 2)),
              `Labour` = mean(Labour > Total / 2),
              `Either grouping needs\na coalition with NZ First` = 
                mean((LabGreen + NZ_First) >= (Total / 2) & (LabGreen <= Total / 2)),
              `Labour + Greens + NZ First\nexact tie with National-led coalition` =
                mean((LabGreen + NZ_First) == Total / 2))
  
  
  
  p3 <- chances %>%
          gather(outcome, prob) %>%
          mutate(outcome = factor(outcome, levels = names(chances)[c(1,2,6,5,3,4)])) %>%
          ggplot(aes(x = outcome, weight = prob, fill = outcome)) +
          geom_bar() +
          geom_text(aes(label = paste0(round(prob * 100, 1), "%"), y = prob +.043), colour = "darkred", size = 3) +
          coord_flip() +
          scale_y_continuous("Chance of happening", label = percent, limits = c(0, 1.05)) +
          theme(legend.position = "none") +
          scale_fill_viridis(discrete = TRUE, option = "C", begin = 0.1, end = 0.9) +
          labs(x = "", caption = "Source: http://freerangestats.info") +
          ggtitle(glue("Probable outcomes for the New Zealand {format(as.Date(ThisElection), '%Y')} General Election"),
                  paste("Modelling based on polls from 2011 to", format(Sys.Date(), "%d %B %Y")))
  
  svg_png(p3, paste0("./output/", prefix, "-final-chances-bar"), 8, 3)
  
  
  
  p4 <- seats %>%
          gather(Party, Number) %>%
          mutate(Party = gsub("_", " ", Party)) %>%
          mutate(Party = fct_reorder(Party, Number)) %>%
          ggplot(aes(x = Number, y = ..density..)) +
          facet_wrap(~Party, scales = "free", ncol = 5) +
          geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.5, colour = NA) +
          labs(x = "Number of seats", y = "Probability",
               caption = "http://freerangestats.info") +
          ggtitle(glue("Simulated election outcomes for {format(as.Date(ThisElection), '%d %B %Y')}"),
                  "Forecasts based on opinion poll trends, calibrated to previous election outcomes")
  
  svg_png(p4, paste0("./output/", prefix, "-final-chances-histogram"), 8, 4)
  
  return(seats)
  
}  
