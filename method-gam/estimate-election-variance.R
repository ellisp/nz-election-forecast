# Idea is to estimate the mean square error, possibly by party,
# on logit scale of the actual election results compared to the
# percentage that was predicted for them.  This then becomes
# the individual randomness element of the simulation model

PollsAll <- polls %>%
  as_tibble() %>%
  left_join(house_effects, by = c("Pollster", "Party")) %>%
  filter(Pollster != "Election result") %>%
  mutate(Bias = ifelse(is.na(Bias), 0, Bias),
         VotingIntention = ifelse(VotingIntention < 0.0005, 0.0005, VotingIntention),
         VotingIntention = logit(VotingIntention) - Bias) %>%
  mutate(PollDate = paste(Pollster, MidDate),
         ID = 1:n()) %>%
  select(Party, VotingIntention, PollDate, MidDate, ElectionYear) %>% 
  spread(Party, VotingIntention, fill = logit(0.0005)) %>%
  mutate(MidDate = as.numeric(MidDate)) 
  
names(PollsAll) <- make.names(names(PollsAll))

mod_all <- gam(list(
  ACT ~ s(MidDate, k = 3),
  Conservative ~ s(MidDate, k = 3),
  Green ~ s(MidDate),
  Labour ~ s(MidDate),
  Mana ~ s(MidDate, k = 3),
  Maori ~ s(MidDate, k = 3),
  National ~ s(MidDate),
  NZ.First ~ s(MidDate),
  United.Future ~ MidDate),
  data = PollsAll, family = mvn(d = 9))

# election results.  Note this snippet is copied from estimate-house-effects,
# should rationalise:
results <- polls %>%
  filter(!ElectionYear %in% range(ElectionYear)) %>%
  filter(Pollster == "Election result")   %>%
  as_tibble() %>%
  select(MidDate, Party, VotingIntention) %>%
  mutate(Party = make.names(Party),
         VotingIntention = logit(VotingIntention))

election_dates <- unique(results$MidDate)
election_predictions <- predict(mod_all, 
                                newdata = data.frame(MidDate = as.numeric(election_dates))) %>%
  as.data.frame() %>%
  cbind(election_dates)
names(election_predictions) <- c("ACT", "Conservative", "Green", "Labour", "Mana",
                                "Maori", "National", "NZ.First", "United.Future", "MidDate")

comparisons <- election_predictions %>%
  gather(Party, Result, -MidDate) %>%
  left_join(results, by = c("Party", "MidDate")) %>%
  mutate(SquaredError = (Result - VotingIntention) ^ 2) %>%
  filter(!is.na(VotingIntention))

# variance is a function of Result, roughly linear from log(var) ~ logit(result)
svg("./output/0085-variance.svg", 8, 7)
ggplot(comparisons, aes(x = Result, y = SquaredError, label = paste(Party, substring(MidDate,1, 4)))) +
  geom_smooth(method = "lm") +
  geom_text(aes(colour = as.ordered(MidDate))) +
  labs(x = "Result (logit scale)")+
  scale_y_log10("Squared Error (on logit scale) from actual election result", label = comma) +
  scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.8, option = "B") +
  ggtitle("Variance compared to election result")
dev.off()
      
mod_var <- lm(log(SquaredError) ~ Result, data = comparisons)
summary(mod_var)

coef(mod_var)
