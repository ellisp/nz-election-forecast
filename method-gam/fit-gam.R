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
parties <- sort(as.character(parties[!parties %in% c("Destiny", "Progressive", "Opportunities", "TOP")]))

svg("./output/exploratory-plot.svg", 8, 6)
PollsElection %>%
  filter(Party %in% parties) %>%
  mutate(Party = fct_drop(Party)) %>%
  mutate(Party = fct_reorder(Party, VotingIntention, fun = max)) %>%
  ggplot(aes(x = MidDate, y = VotingIntention)) +
  geom_point(aes(colour = Pollster)) +
  geom_line(aes(colour = Pollster)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~Party, scales = "free_y") +
  scale_y_continuous("logit(Voting Intention), after adjusting for house effects\n",
                     sec.axis = sec_axis(~inv.logit(.) * 100,
                                         name = "Voting Intention, after adjusting for house effects\n")) 
dev.off()


#===========correlations (on logit scale)================
polls_w <- PollsElection %>%
  filter(Party %in% parties) %>%
  mutate(PollDate = paste(Pollster, MidDate),
         ID = 1:n()) %>%
  select(Party, VotingIntention, PollDate, MidDate) %>% 
  spread(Party, VotingIntention, fill = logit(0.0005)) %>%
  mutate(MidDate = as.numeric(MidDate))

cors <- cor(polls_w[ , -(1:2)])

svg("output/correlation-polls.svg", 8, 7)
print(ggcorr(polls_w[ , -(1:2)], label = TRUE, label_alpha = TRUE, label_round = 2) +
  ggtitle("Correlations in polling numbers, 2017 election"))
dev.off()

#===============modelling and predictions==============

# There aren't enough points to let all parties have fully flexible 
# smoothing terms, so for smaller parties we reduce the degrees of freedom
# available with k=3, and for United.Future we force the relationship
# to be linear on the logit scale.  The four biggest parties get unrestricted
# use of s() so they can be flexible for changes over time.  At time of
# writing this makes most difference for Labour.

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

#====================graphic showing predicted range of party vote=====================
full_period <- data.frame(
  MidDate = seq(from = min(polls_w$MidDate), to = electionday$MidDate, length.out = 1000)
)
mod_pred_all <- predict(mod, newdata = full_period, se.fit = TRUE)

f <- mod_pred_all$fit
se <- mod_pred_all$se.fit
colnames(f) <- colnames(se) <- parties

se <- se %>%
  as_tibble %>%
  cbind(full_period) %>%
  gather(party, error, -MidDate)

fitted <- f %>%
  as_tibble %>%
  cbind(full_period) %>%
  gather(Party, Vote, -MidDate) %>%
  mutate(se = se$error,
         Lower = inv.logit(Vote - 1.96 * se), 
         Upper = inv.logit(Vote + 1.96 * se),
         Vote = inv.logit(Vote)) %>%
  as_tibble() %>%
  select(-se)


svg("./output/gam-vote-predictions.svg", 9, 6)
print(fitted %>%
  ggplot(aes(x = as.Date(MidDate, origin = "1970-01-01"), y = Vote)) +
  facet_wrap(~Party, scales = "free_y") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.1, fill = "darkgreen") +
  geom_line() +
  scale_y_continuous(label = percent) +
  geom_point(data = filter(PollsElection, Party %in% parties), 
                           aes(y = inv.logit(VotingIntention), x = MidDate),
             size = 0.8, colour = "steelblue") +
  labs(x = "", caption = "Source: https://ellisp.github.io") +
  ggtitle("Predicted party vote for the 23 September 2017 New Zealand General Election",
          "Points represent individual polls; adjusted for previous performance in predicting election results"))
dev.off()  

#=======================point prediction for election day================
mod_pred_elect <- predict(mod, newdata = electionday, se.fit = TRUE)

pred_votes <- data.frame(
  Lower = as.vector(mod_pred_elect[["fit"]] -  1.96 * mod_pred_elect[["se.fit"]]),
  Midpoint = as.vector(mod_pred_elect[["fit"]]),
  Upper = as.vector(mod_pred_elect[["fit"]] +  1.96 * mod_pred_elect[["se.fit"]])) %>%
  map_df(function(x){round(inv.logit(x) * 100, 1)}) %>%
  mutate(Party = parties)

knitr::kable(pred_votes[ , c(4,1,2,3)])
