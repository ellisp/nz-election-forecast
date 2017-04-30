house_colours <- c("black", brewer.pal(3, "Set1"))
names(house_colours) <-   c("Election result", "Reid Research", "Colmar Brunton", "Roy Morgan")

#===========house effects on logit scale from previous elections=================
# vector of just the seven main parties with a track record to use
parties <- polls %>%
  filter(ElectionYear == 2017) %>%
  distinct(Party) %>%
  filter(!Party %in% c("Destiny", "Progressive", "Mana", "Conservative", "Opportunities", "TOP")) %$%
  Party

house_bias3 <- function(elect_years, pollsters){
  # Estimates house effects on the *logit* scale.
  # depends on these objects being in environment:
  # polls, parties
  # Note this is different to house_bias() from a previous post,
  # which drew graphics, and estimated bias on the original scale.
  
  houses <- expand.grid(elect_years, pollsters, stringsAsFactors = FALSE)
  names(houses) <- c("ElectionYear", "Pollster")
  
  for(j in 1:length(parties)){
    the_party = parties[j]
    
    # election results:
    results <- polls %>%
      filter(ElectionYear %in% elect_years & ElectionYear != 2002) %>%
      filter(Pollster == "Election result")  %>%
      filter(Party == the_party) 
    
    
    for(i in 1:length(elect_years)){
      
      # Note we include *all* pollsters in the data for fitting the model
      thedata <- polls %>%
        filter(ElectionYear == elect_years[i] & Pollster != "Election result") %>%
        filter(Party == the_party)
      
      mod <- gam(VotingIntention ~ s(as.numeric(MidDate)) + Pollster, 
                 family = "quasibinomial", data = thedata)
      
      # for predicting values, we only take the pollsters we have an interest in:
      preddata <- data.frame(MidDate = as.numeric(results[i, "MidDate"]), Pollster = pollsters)
      
      # house effect is shown by the amount the predicted value from polling
      # is *more* than the actual vote.  So a positive score means the poll
      # overestimated the actual vote:
      houseeffects <- predict(mod, newdata = preddata, type = "link") -
        logit(results[i, "VotingIntention"])
      houses[houses$ElectionYear == elect_years[i], the_party] <- houseeffects
    }
    
  }   
  
  output <- data_frame(Party = character(), Pollster = character(),
                       Bias = numeric(), SampVar = numeric())
  
  for(i in pollsters){
    for(j in parties){
      disc <- houses[houses$Pollster == i, j]
      x <- list(disc = disc, N = length(disc))
      fit <- stan(file = 'method-gam/estimate-house-effects.stan', data = x, control = list(adapt_delta = 0.995))
      fit_e <- extract(fit)$house_effect
      tmp <- data_frame(
        Party = j,
        Pollster = i,
        Bias = mean(fit_e), 
        SampVar = sd(fit_e) ^ 2)
      output <- rbind(output, tmp)
    }
  }
  output %>%
    arrange(Party, Pollster) %>%
    return()
  }

hb1 <- house_bias3(elect_years = c(2005, 2008, 2011, 2014),
                   pollsters   = c("Colmar Brunton", "Roy Morgan"))      

# Note we include all three pollsters even though we wi
hb2 <- house_bias3(elect_years = c(2011, 2014),
                   pollsters    = c("Reid Research"))      

house_effects <- hb2 %>%
  rbind(hb1) %>%
  arrange(Party, Pollster) %>%
  mutate(SE = sqrt(SampVar))

