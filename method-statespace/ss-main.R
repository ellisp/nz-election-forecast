library(tidyverse)
library(scales)
library(nzelect)
library(forcats)
library(rstan)
library(directlabels)

rstan_options(auto_write = TRUE)
options(mc.cores = 7)


parties_v2 <- c(parties_v, Maori = "#EF4A42", Other = "#151A61")

elections <- polls %>%
  mutate(Party = gsub("M.ori", "Maori", Party)) %>%
  mutate(Party = fct_other(Party, keep = c("ACT", "National", "NZ First", "Labour", "Green", "Maori"))) %>%
  group_by(Party, Pollster, MidDate, ElectionYear) %>%
  summarise(VotingIntention = sum(VotingIntention)) %>%
  filter(Pollster == "Election result") %>%
  spread(Party, VotingIntention) %>%
  tail(2) %>%
  ungroup() %>%
  select(ACT:Other)

polls2 <- polls %>%
  mutate(Party = gsub("M.ori", "Maori", Party)) %>%
  mutate(Party = fct_other(Party, keep = c("ACT", "National", "NZ First", "Labour", "Green", "Maori"))) %>%
  group_by(Party, Pollster, MidDate, ElectionYear) %>%
  summarise(VotingIntention = sum(VotingIntention, na.rm = TRUE)) %>%
  mutate(VotingIntention = ifelse(is.na(VotingIntention), 0, VotingIntention)) %>%
  filter(Pollster != "Election result") %>%
  filter(ElectionYear %in% c(2014, 2017)) %>%
  # SSI only have a single poll, which stops the model converging, so we leave them out for now:
  filter(Pollster != "SSI") %>%
  spread(Party, VotingIntention, fill = 0) %>%
  ungroup() %>%
  mutate(MidDateNumber = as.numeric(MidDate - as.Date("2011-11-25"))) # election was 26 November 2011
  
pollsters <- unique(polls2$Pollster)
# if the number of pollsters isn't six we need to make some hard coded changes both here
# and in ss-vectorized.stan
expect_equal(length(pollsters), 6)

polls3 <- lapply(pollsters, function(x){
  dplyr::filter(polls2, Pollster == x)
})

parties_ss <- names(elections)

election_dates <-  as.Date(c("2011-11-26", "2014-09-20", "2017-09-23"))
days_between_elections <- as.integer(diff(election_dates)) + 1


# estimate the standard errors.  Note we are pretending they all have a sample size of 800 -
# which the main five do, but not some of the smaller ones.  Improvement would be to better deal with this.

all_ses <- polls2 %>%
  select(Pollster, ACT:Other) %>%
  gather(Party, p, -Pollster) %>%
  group_by(Pollster, Party) %>%
  summarise(p = mean(p),
            se = sqrt(p * (1-p) / 800))
ses3 <- lapply(pollsters, function(x){
  dplyr::filter(all_ses, Pollster == x)
})

# The pollster data is a big list with one element per pollster
d1 <- list(mu_start = as.numeric(elections[1, ]), 
           mu_finish = as.numeric(elections[2, ]), 
           
           n_parties = length(parties_ss),
           n_days = days_between_elections, 
           # multiply the variance of all polls by 2.  See my blog post of 9 July 2017.
           inflator = sqrt(2),

           y1_n = nrow(polls3[[1]]),
           y1_values = polls3[[1]][ , 4:10],
           y1_days = as.numeric(polls3[[1]]$MidDateNumber),
           y1_se = ses3[[1]]$se,
           
           y2_n = nrow(polls3[[2]]),
           y2_values = polls3[[2]][ , 4:10],
           y2_days = as.numeric(polls3[[2]]$MidDateNumber),
           y2_se = ses3[[2]]$se,
           
           y3_n = nrow(polls3[[3]]),
           y3_values = polls3[[3]][ , 4:10],
           y3_days = as.numeric(polls3[[3]]$MidDateNumber),
           y3_se = ses3[[3]]$se,
           
           y4_n = nrow(polls3[[4]]),
           y4_values = polls3[[4]][ , 4:10],
           y4_days = as.numeric(polls3[[4]]$MidDateNumber),
           y4_se = ses3[[4]]$se,
           
           y5_n = nrow(polls3[[5]]),
           y5_values = polls3[[5]][ , 4:10],
           y5_days = as.numeric(polls3[[5]]$MidDateNumber),
           y5_se = ses3[[5]]$se,
           
           y6_n = nrow(polls3[[6]]),
           y6_values = polls3[[6]][ , 4:10],
           y6_days = as.numeric(polls3[[6]]$MidDateNumber),
           y6_se = ses3[[6]]$se,
           
           # y7_n = nrow(polls3[[7]]),
           # y7_values = polls3[[7]][ , 4:10],
           # y7_days = as.numeric(polls3[[7]]$MidDateNumber),
           # y7_se = ses3[[7]]$se,
           
           n_pollsters = 6)

# The Listener-Bauer sample sizes are 1816, 1175 and 1528, which are bigger than the others,
# so we manually make its standard errors a little smaller.  They are first in the list of
# pollsters by alphabetical order:
d1$y1_se <- d1$y1_se * sqrt(800 / 1500)

# good discussion here on iterations and chains https://groups.google.com/forum/#!topic/stan-users/5WG51xKNSbA
# The below is used on my 8 core machine
system.time({
  m1 <- stan(file = "method-statespace/ss-vectorized.stan", data = d1, 
             chains = 4, iter = 1200, control = list(max_treedepth = 10))
}) 
# c. 6 hours original; 3.5 hours when standard errors only calculated once in advance. 20 minutes when re-parameterised. 
# Back up to 80 minutes when made the innovations covary with eachother rather than independent

source("method-statespace/ss-diagnostics.R")

s1 <- summary(m1, pars = "mu")$summary %>%
  as_tibble() %>%
  mutate(Party = rep(parties_ss, sum(days_between_elections)),
         day = rep(1:sum(days_between_elections), each = length(parties_ss)),
         day = as.Date(day, origin = "2011-11-25"))

# summary(m1, pars = "s")$summary
summary(m1, pars = "d")$summary[ ,"mean"]
round(summary(m1, pars = "sigma")$summary[ ,"mean"], 6)



p1 <- s1 %>%
  ggplot(aes(x = day, y = mean, colour = Party, fill = Party)) +
  geom_point(data = gather(polls2, Party, VotingIntention, -Pollster, -MidDate, -ElectionYear, -MidDateNumber),
             aes(x = MidDate, y = VotingIntention), colour = "black", size = 0.5) +
  geom_line(colour = "black") +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3, colour = NA) +
  scale_colour_manual(values = parties_v2) +
  scale_fill_manual(values = parties_v2) +
  labs(x = "", y = "Party vote") +
  facet_wrap(~Party, scales = "free_y") +
  theme(legend.position= "none") +
  scale_y_continuous(label = percent) +
  geom_vline(xintercept = as.numeric(election_dates), colour = "black") +
  ggtitle("Voting intention from the 2011 to the 2017 elections",
          paste("State-space modelling based on polls from 2011 to", format(Sys.Date(), "%d %B %Y"))) +
  labs(caption = "https://ellisp.github.io/elections/elections.html")


svg("./output/state-space-ribbons.svg", 10, 6)  
print(p1)
grid.text(0.65, 0.2, label = "Vertical lines indicate elections.  Points show poll results.
Estimates of latent voting intention take into account past under 
and over-estimates of polling firms.",
          gp = gpar(fontfamily = thefont))
dev.off()


# standard deviation - in percentage points (ie not proportions)
# of the daily innovations
summary(m1, pars = "sigma")$summary %>%
  (function(x){round( x * 100, 3)}) %>%
  as_tibble() %>%
  select(mean, se_mean) %>%
  mutate(party = parties_ss)
# The main difference between the GAM model and the state space model
# is that the state space model lets the major parties latent vote
# change much quicker.  So as at end of June 2017, the National vote
# bounces up (along with the budget bounce) in the state space model,
# whereas the GAM model still has them trending downwards.


# house effects
svg("./output/state-space-house-effects.svg", 8, 6)
print(
data.frame(d = round(summary(m1, pars = "d")$summary[, "mean"] * 100, 2),
           pollster = rep(pollsters, each = length(parties_ss)),
           party = rep(parties_ss, length(pollsters))) %>%
  arrange(party) %>%
  ggplot(aes(y = party, colour = pollster, x = d)) +
  geom_point(size = 2) +
  labs(x = "Average house effect (positive numbers mean the pollster over-estimates vote for that party)",
       y = "", colour = "")
)
dev.off()

# extract the simulations for the final election day (and make up for the the three tiny parties)
sims_ss <- data.frame(rstan::extract(m1, "mu")$mu[ , sum(days_between_elections), ]) %>%
  mutate(Conservative = rbeta(n(), 1, 3) / 100,
         `United Future` = rbeta(n(), 1, 3) / 100,
         Mana = rbeta(n(), 1, 3) / 100)
names(sims_ss)[1:length(parties_ss)] <- parties_ss
sims_ss <- select(sims_ss, -Other)

seats_ss <- simulate_seats(sims_ss, prefix = "state-space")
