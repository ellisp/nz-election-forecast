
# Depends on fit-gam.R

#==========simulations============
n <- 10000


# estimated cov matrix from model.  This is less than when estimated
# directly from the logit of the polls, because (I think) this is the 
# covariance in the underlying moving mean, not the noise from the polls
#
mod_cov <- solve(crossprod(mod$family$data$R)) 

se <- as.vector(mod_pred_elect[["se.fit"]])
sims <- inv.logit(MASS::mvrnorm(n = n, 
                                mu = mod_pred_elect[["fit"]],
                                Sigma = se %*% t(se) * cov2cor(mod_cov))) %>%
  as_tibble()
names(sims) <- parties

sims_tidy <- sims %>%
  mutate(ID = 1:n()) %>%
  gather(Party, Vote, -ID) %>%
  group_by(ID) %>%
  mutate(Vote = Vote / sum(Vote))

svg("./output/gam-vote-predictions-density.svg", 9, 6)
sims_tidy %>%
  ggplot(aes(x = Vote)) +
  geom_density(fill = "darkgreen", alpha = 0.1, colour = "grey50") +
  facet_wrap(~Party, scales = "free") +
  scale_x_continuous(label = percent) +
  labs(x = "Predicted party vote on election day", caption = "Source: https://ellisp.github.io") +
  ggtitle("Predicted party vote for the 23 September 2017 New Zealand General Election",
          "Simulations based on predictions from polling data")
dev.off()

#===============simulate electorates================

# allocating vote for Maori seats done based on discussion at
# http://www.newshub.co.nz/home/politics/2017/02/what-the-mana-maori-deal-would-ve-meant-in-the-2014-election.html
# For lack of any better way of doing it, I give Labour a probability of winning proportionate to their
# votes in the 2014 election compared to the combined Mana/Maori party votes
m_votes_2014 <- data_frame(
  labour = c(9712, 7533, 8089, 9753, 8445, 12191, 5837),
  mana_maori = c(11548, 8695, 8475, 8928, 6887, 7612, 15208)
) %>%
  mutate(
    mana_maori = mana_maori * 0.9,
    prob_lab = labour / (mana_maori + labour),
    prob_oth = 1 - prob_lab)


# a filler data frame of the three parties that don't get any simulated electorate seats.
# Note that NZ Firs isn't a certainty to get their seat, but it doesn't matter as they are 
# almost certainly above the 5% threshold anyway, so we don't bother to simulate Northland
filler <- data.frame(
  party = c("Conservative", "Green", "NZ First"),
  seats = c(0, 0, 1),
  sim = rep(1:n, each = 3)
)

# see https://en.wikipedia.org/wiki/M%C4%81ori_electorates for the true names of the Maori electorates.
# For convenience I call them m1 : m7.
# Also note - the procedure below often gives zero electorate seats to National.  This doesn't
# impact on seat allocation as they always exceed the 5% party vote threshhold; they only
# need to be in the simulation at all for the off chance they take Epsom off ACT.
electorate_sims <- data_frame(
  orahiu = sample(c("United Future", "Labour"), prob = c(0.6, 0.4), size = n, replace = TRUE),
  epsom = sample(c("ACT", "National", "Labour"), prob = c(0.8, 0.1, 0.1), size = n, replace = TRUE),
  m1 = sample(c("Labour", "Mana"), prob = m_votes_2014[1, 3:4], size = n, replace = TRUE),
  m2 = sample(c("Labour", "Maori"), prob = m_votes_2014[2, 3:4], size = n, replace = TRUE),
  m3 = sample(c("Labour", "Maori"), prob = m_votes_2014[3, 3:4], size = n, replace = TRUE),
  m4 = sample(c("Labour", "Maori"), prob = m_votes_2014[4, 3:4], size = n, replace = TRUE),
  m5 = sample(c("Labour", "Maori"), prob = m_votes_2014[5, 3:4], size = n, replace = TRUE),
  m6 = sample(c("Labour", "Maori"), prob = m_votes_2014[6, 3:4], size = n, replace = TRUE),
  m7 = sample(c("Labour", "Maori"), prob = m_votes_2014[7, 3:4], size = n, replace = TRUE)
  ) %>%
  mutate(sim = 1:n()) %>%
  gather(seat, party, -sim) %>%
  group_by(party, sim) %>%
  summarise(seats = n()) %>%
  ungroup() %>%
  rbind(filler) %>%
  spread(party, seats, fill = 0) 



#============allocate seats==================


seats <- t(sapply(1:n, function(i){
  allocate_seats(votes = as.numeric(sims[i, ]), 
                 electorate = as.numeric(electorate_sims[i, -1]),
                 parties = parties)$seats_v
})) %>%
  as_tibble()

names(seats) <- gsub("M.ori", "Maori", names(seats))
names(seats) <- gsub("NZ First", "NZ_First", names(seats))

seats <- seats %>%
  mutate(NatCoal = ACT + Conservative + National + `United Future` + Maori,
         LabGreen = Labour + Green,
         LabGreenMana = Labour + Green + Mana,
         LabGreenManaNZFirst = Labour + Green + Mana + NZ_First)
seats$Total <- apply(seats[ , 1:9], 1, sum)


#==================presentation=====================

p <- seats %>%
  select(National, NatCoal, LabGreen, LabGreenMana, LabGreenManaNZFirst) %>%
  gather(Coalition, Seats) %>%
  ggplot(aes(x = Seats, colour = Coalition, fill = Coalition)) +
#  geom_vline(xintercept = 60, colour = "black") +
  geom_density(alpha = 0.5) 

direct.label(p)

# This graph is interestin.
# note that the correlations here are much further from zero than the 
# correlations of votes, because of the seats algorithm
seats %>%
  mutate(Other = ACT + `United Future` + Conservative + Mana + Maori) %>%
  dplyr::select(Green, Labour, National, NZ_First, Other) %>%
  ggpairs() +
  ggtitle(paste("Possible outcomes for number of seats", ThisElection))

chances <- seats %>%
  summarise(`Nationals win by themselves` = mean(National > Total / 2),
            `National led coalition as per 2014` = mean(NatCoal > Total / 2 & National <= Total / 2),
            `Labour + Green win by themselves` = mean(LabGreen > Total / 2),
            `Labour + Green + Mana win` = mean(LabGreen + Mana > Total / 2),
            `NZ First get leverage` = mean((Green + Labour + Mana + NZ_First) >= Total / 2) - `Labour + Green + Mana win`)

svg("./output/gam-final-chances-bar.svg", 8, 5)
chances %>%
  gather(outcome, prob) %>%
  mutate(outcome = fct_reorder(outcome, prob)) %>%
  ggplot(aes(x = outcome, weight = prob)) +
  geom_bar(fill = "steelblue") +
  geom_text(aes(label = paste0(round(prob * 100, 1), "%"), y = prob +.03), colour = "darkred") +
  coord_flip() +
  scale_y_continuous("Chance of happening", label = percent) +
  labs(x = "", caption = "Source: https://ellisp.github.io") +
  ggtitle("Probability of different outcomes for the New Zealand 2017 General Election",
          paste("Modelling based on polls as at", format(Sys.Date(), "%d %B %Y")))
dev.off()
