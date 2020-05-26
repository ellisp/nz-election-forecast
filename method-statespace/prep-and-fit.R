
election_dates <-  as.Date(c("2011-11-26", "2014-09-20", "2017-09-23", ThisElection))
days_between_elections <- as.integer(diff(election_dates)) + 1



parties_v2 <- c(parties_v, Maori = "#EF4A42", Other = "#151A61")

# election results for the elections OTHER than the final one (which we are forecasting)
elections <- polls %>%
  mutate(Party = gsub("M.ori", "Maori", Party)) %>%
  mutate(Party = fct_other(Party, keep = c("ACT", "National", "NZ First", "Labour", "Green", "Maori"))) %>%
  filter(MidDate %in% election_dates[-length(election_dates)]) %>%
  group_by(Party, Pollster, MidDate, ElectionYear) %>%
  summarise(VotingIntention = sum(VotingIntention)) %>%
  filter(Pollster == "Election result") %>%
  spread(Party, VotingIntention) %>%
  ungroup() %>%
  arrange(MidDate) %>%
  select(ACT:Other)

polls2 <- polls %>%
  # Note enough YouGov polls to determine the pollster effect. Adjust this once we have say 3 polls from them.
  # This will mean manual changes to the Stan program as six pollsters is hard coded in.
  filter(Pollster != "YouGov") %>%
  mutate(Party = gsub("M.ori", "Maori", Party)) %>%
  mutate(Party = fct_other(Party, keep = c("ACT", "National", "NZ First", "Labour", "Green", "Maori"))) %>%
  group_by(Party, Pollster, MidDate, ElectionYear) %>%
  summarise(VotingIntention = sum(VotingIntention, na.rm = TRUE)) %>%
  mutate(VotingIntention = ifelse(is.na(VotingIntention), 0, VotingIntention)) %>%
  filter(Pollster != "Election result") %>%
  filter(ElectionYear %in% year(election_dates[-1])) %>%
  # SSI only have a single poll, which stops the model converging, so we leave them out for now:
  filter(Pollster != "SSI") %>%
  filter(Pollster != "Horizon Research") %>%
  spread(Party, VotingIntention, fill = 0) %>%
  ungroup() %>%
  # days since the first election in our model:
  mutate(MidDateNumber = 1 + as.numeric(MidDate - election_dates[1])) %>%
  filter(MidDate < max(election_dates))
  
pollsters <- unique(polls2$Pollster)
# if the number of pollsters isn't six we need to make some hard coded changes both here
# and in ss-vectorized.stan
expect_equal(length(pollsters), 6)

# As we have a specific dummy variable later on for a Reid change in method, check that
# they are number five in the vector
expect_equal(pollsters[5], "Reid Research")

polls3 <- lapply(pollsters, function(x){
  dplyr::filter(polls2, Pollster == x)
})

# Party names in alphabetical order as per the columns in 'elections'
parties_ss <- names(elections)


# estimate the standard errors.  Note we are pretending they all have a sample size of 1000 -
# which the main five do, but not some of the smaller ones.  Improvement would be to better deal with this.

all_ses <- polls2 %>%
  select(Pollster, ACT:Other) %>%
  gather(Party, p, -Pollster) %>%
  group_by(Pollster, Party) %>%
  summarise(p = mean(p),
            se = sqrt(p * (1-p) / 1000))
ses3 <- lapply(pollsters, function(x){
  dplyr::filter(all_ses, Pollster == x)
})

# The pollster data is a big list with one element per pollster
d1 <- list(mu_elect1 = as.numeric(elections[1, ]), 
           mu_elect2 = as.numeric(elections[2, ]),
           mu_elect3 = as.numeric(elections[3, ]), 
           expected_mu_government = 0.369,
           expected_sigma_government = 0.1,
           party_govt_number = which(parties_ss == "Labour"),
           
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
           # next row is a dummy variable specially for Reid Research's change in methodology:
           reid_method = as.numeric(polls3[[5]]$MidDate >= as.Date("2017-01-01")),
           
           y6_n = nrow(polls3[[6]]),
           y6_values = polls3[[6]][ , 4:10],
           y6_days = as.numeric(polls3[[6]]$MidDateNumber),
           y6_se = ses3[[6]]$se,
           
           n_pollsters = 6)

# The Listener-Bauer sample sizes are 1816, 1175 and 1528, which are bigger than the others,
# so we manually make its standard errors a little smaller.  They are first in the list of
# pollsters by alphabetical order:
d1$y1_se <- d1$y1_se * sqrt(800 / 1500)

# good discussion here on iterations and chains https://groups.google.com/forum/#!topic/stan-users/5WG51xKNSbA
# The below is used on my 8 core machine.  For production chains=4, iter=1200
system.time({
  m1 <- stan(file = "method-statespace/ss-vectorized.stan", data = d1, 
             chains = 4, iter = 1500, control = list(max_treedepth = 15))
}) 
# with 1200 iterations per chain this takes about 10 hours 



save(m1, file = glue("data/m1-{Sys.Date()}.rda"), compress = "xz")


