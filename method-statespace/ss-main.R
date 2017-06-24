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

parties <- names(elections)

days_between_elections <- as.integer(diff(as.Date(c("2011-11-26", "2014-09-20", "2017-09-23")))) + 1

days_between_elections <- c(30, 25)

d1 <- list(mu_start = as.numeric(elections[1, ]), 
           mu_finish = as.numeric(elections[2, ]), 
           n_parties = length(parties),
           n_days = days_between_elections)

m1 <- stan(file = "method-statespace/ss-simple.stan", data = d1, chains = 1)

s1 <- summary(m1, pars = "mu")$summary %>%
  as_tibble() %>%
  mutate(party = rep(parties, sum(days_between_elections)),
         day = rep(1:sum(days_between_elections), each = length(parties)))

p1 <- s1 %>%
  ggplot(aes(x = day, y = mean, colour = party, fill = party)) +
  geom_line() +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.7) +
  scale_colour_manual(values = parties_v2) +
  scale_fill_manual(values = parties_v2)
p1


summary(m1, pars = "sigma")
parties
