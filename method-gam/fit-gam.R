# This script is the workhorse that fits the actual model predicting
# party vote, on a logit scale, corrected for house effects.
# Depends on estimate-house-effects.R being run first


#=========setup======================

ThisElection <- "2017-09-23"

electionday <- data_frame(
  MidDate = as.numeric(as.Date(ThisElection))
)

PollsElection <- polls %>%
  as_tibble() %>%
  filter(ElectionYear == substring(ThisElection, 1, 4)) %>%
  left_join(house_effects, by = c("Pollster", "Party")) %>%
  # we don't have bias estimates for Mana or Conservative parties,
  # or for pollsters other than Colmar Brunton, Reid Research and 
  # Roy Morgan.  So replace NA in bias with zero:
  mutate(Bias = ifelse(is.na(Bias), 0, Bias),
         VotingIntention = ifelse(VotingIntention < 0.0005, 0.0005, VotingIntention),
         VotingIntention = logit(VotingIntention) - Bias)
# note that sum(inv.logit(VotingIntention)) now no longer necessarily 
# adds to 100, because of the bias corrections

parties <- unique(PollsElection$Party)
parties <- sort(as.character(parties[!parties %in% c("Destiny", "Progressive")]))

svg("./output/exploratory-plot.svg", 8, 6)
PollsElection %>%
  filter(Party %in% parties) %>%
  mutate(Party = fct_drop(Party)) %>%
  #   mutate(Party = fct_reorder(Party, VotingIntention, fun = max)) %>%
  ggplot(aes(x = MidDate, y = VotingIntention)) +
  geom_point(aes(colour = Pollster)) +
  geom_line(aes(colour = Pollster)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~Party, scales = "free_y") +
  scale_y_continuous("logit(Voting Intention), after adjusting for house effects\n",
                     sec.axis = sec_axis(~inv.logit(.) * 100,
                                         name = "Voting Intention, after adjusting for house effects\n")) 
dev.off()


#===========correlations================
polls_w <- PollsElection %>%
  filter(Party %in% parties) %>%
  mutate(PollDate = paste(Pollster, MidDate),
         ID = 1:n()) %>%
  select(Party, VotingIntention, PollDate, MidDate) %>% 
  spread(Party, VotingIntention, fill = 0) %>%
  mutate(MidDate = as.numeric(MidDate))

cors <- cor(polls_w[ , -(1:2)])



# might want to do a graphic here showing the correlations
#===============modelling and predictions==============

names(polls_w) <- make.names(names(polls_w))
mod <- gam(list(
  ACT ~ s(MidDate, k = 3),
  Conservative ~ s(MidDate, k = 3),
  Green ~ s(MidDate),
  Labour ~ s(MidDate),
  Mana ~ s(MidDate, k = 3),
  Maori ~ s(MidDate, k = 3),
  National ~ s(MidDate),
  NZ.First ~ s(MidDate),
  United.Future ~ MidDate
),   data = polls_w,   family = mvn(d = 9))

mod_pred <- predict(mod, newdata = electionday, se.fit = TRUE)
summary(mod)
str(mod)
plot(mod, pages = 1, shade = TRUE)

pred_votes <- data.frame(
  Lower = as.vector(mod_pred[["fit"]] -  1.96 * mod_pred[["se.fit"]]),
  Midpoint = as.vector(mod_pred[["fit"]]),
  Upper = as.vector(mod_pred[["fit"]] +  1.96 * mod_pred[["se.fit"]])) %>%
  map_df(function(x){round(inv.logit(x) * 100, 1)}) %>%
  mutate(Party = parties)

pred_votes