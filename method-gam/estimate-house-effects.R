house_colours <- c("black", brewer.pal(3, "Set1"))
names(house_colours) <-   c("Election result", "Reid Research", "Colmar Brunton", "Roy Morgan")
#===========house effects on logit scale from previous elections=================
# vector of just the seven main parties with a track record to use
parties <- polls %>%
  filter(ElectionYear == 2017) %>%
  distinct(Party) %>%
  filter(!Party %in% c("Destiny", "Progressive", "Mana", "Conservative", "Opportunities", "TOP")) %$%
  Party

house_bias2 <- function(elect_years, pollsters, plot = FALSE, shrinkage = 0.7){
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
  
  if(plot){
    p <- houses %>%
      gather(Party, `Polling overestimate`, -ElectionYear, -Pollster) %>%
      ggplot(aes(x = ElectionYear, y = `Polling overestimate`, colour = Pollster)) +
      geom_hline(yintercept = 0, colour = "black") +
      geom_point() +
      geom_line() +
      facet_wrap(~Party, ncol = 4) +
      scale_colour_manual(values = house_colours) +
      scale_x_continuous("Election year", breaks = c(2005, 2008, 2011, 2014), limits = c(2004, 2015)) +
      scale_y_continuous("Polling overestimate (logit scale)") +
      theme(legend.position = c(0.9, 0.18)) +
      ggtitle("Statistical forecast of election compared to actual result",
              "Forecasts use time series methods based on pollsters' results, are not actual pollsters' forecasts") +
      labs(caption = "Source: polls data collected by Wikipedia, available in the {nzelect} R package")
    
    print(p)
  }
  
  houses_av <- houses %>%
    gather(Party, BiasOrig, -ElectionYear, -Pollster) %>%
    group_by(Party, Pollster) %>%
    summarise(Bias = mean(BiasOrig) * shrinkage,
              SampVar = var(BiasOrig) / n())
  
  return(houses_av)
}

hb1 <- house_bias2(elect_years = c(2005, 2008, 2011, 2014),
                   pollsters   = c("Colmar Brunton", "Roy Morgan"),
                   plot = FALSE)      

hb2 <- house_bias2(elect_years = c(2011, 2014),
                   pollsters    = c("Reid Research", "Colmar Brunton", "Roy Morgan"),
                   plot = FALSE)      

house_effects <- hb2 %>%
  filter(Pollster == "Reid Research") %>%
  rbind(hb1) %>%
  arrange(Party, Pollster)

house_effects_vars <- house_effects %>%
  group_by(Party) %>%
  summarise(SE = sqrt(sum(SampVar))) 
