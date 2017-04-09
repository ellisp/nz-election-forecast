
# Depends on fit-gam.R

#==========simulations============
set.seed(123)
n <- 1000


# estimated cov matrix from model.  
mod_cov <- solve(crossprod(mod$family$data$R)) 

# estimated standard error for the predicted values.  This bit is uncontroversial.
se <- as.vector(mod_pred_elect[["se.fit"]])

# In my first iterations I was using sigma1 as the covariance matrix for 
# simulations.  But now I think this is understating the true randomness we should expect
# sigma1 <- se %*% t(se) * cov2cor(mod_cov)

# This second method takes the covariance of the residuals (mod_cov) and
# adds to it the uncertainty from the prediction.  It ends up being too *much*
# variance, because the election is treated as just another small sample poll
# sigma2 <- mod_cov 
#diag(sigma2) <- diag(sigma2) + sqrt(se)

# The third method takes the observed mean squared error from previous elections compared to
# where a gam fits the lines (see estimate-election-variance.R) and treats that as the 
# individual variance of the actual election observation.  This is added to the sum of the
# standard errors for the prediction of the latent party vote variable to create an expected
# standard deviation of the distribution for each party mean.  The correlation matrix
# from the GAM predicting this year's eleciton is used as a basis, scaled up by this standard
# deviation, to create a new covariance matrix
se3 <- as.vector(sqrt(se ^ 2 + pmin(exp(coef(mod_var)[1] + coef(mod_var)[2] * mod_pred_elect[["fit"]]), 1)))
sigma3 <- se3 %*% t(se3) * cov2cor(mod_cov)

# round(sigma1, 2) # way too small
# round(sigma2, 2) # too big
# round(sigma3, 3) # generally (not always) in between
sims <- inv.logit(MASS::mvrnorm(n = n, 
                                mu = mod_pred_elect[["fit"]],
                                Sigma = sigma3)) %>%
  as_tibble()
names(sims) <- parties

sims_tidy <- sims %>%
  mutate(ID = 1:n()) %>%
  gather(Party, Vote, -ID) %>%
  group_by(ID) %>%
  mutate(Vote = Vote / sum(Vote))

svg("./output/gam-vote-predictions-density.svg", 9, 6)
print(sims_tidy %>%
  ggplot(aes(x = Vote)) +
  geom_density(fill = "darkgreen", alpha = 0.1, colour = "grey50") +
  facet_wrap(~Party, scales = "free") +
  scale_x_continuous(label = percent) +
  labs(x = "Predicted party vote on election day", 
       y = "Likelihood",
       caption = "Source: https://ellisp.github.io") +
  ggtitle("Predicted party vote for the 23 September 2017 New Zealand General Election",
          "Simulations based on predictions from polling data")
)
dev.off()

#===============simulate electorates================

# allocating vote for Maori seats done based on discussion at
# http://www.newshub.co.nz/home/politics/2017/02/what-the-mana-maori-deal-would-ve-meant-in-the-2014-election.html
# For lack of any better way of doing it, I give Labour a probability of winning proportionate to their
# votes in the 2014 election compared to the combined Mana/Maori party votes, having knocked 10% off
# those Mana/Maori party votes (this is the magic parameter "0.9" in the below - but I tried reducing this
# as much as to 0.3 ie a collapse in the Maori/Mana vote, without changing the substantive conclusions 
# from the overall simulation)
m_votes_2014 <- data_frame(
  labour = c(9712, 7533, 8089, 9753, 8445, 12191, 5837),
  mana_maori = c(11548, 8695, 8475, 8928, 6887, 7612, 15208)
) %>%
  mutate(
    mana_maori = mana_maori * 0.9,
    prob_lab = labour / (mana_maori + labour),
    prob_oth = 1 - prob_lab)


# a filler data frame of the three parties that don't get any simulated electorate seats.
# Note that NZ First isn't really a certainty to get their seat, but it doesn't matter as they are 
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
         LabGreenNZFirst = Labour + Green + NZ_First,
         NatCoalNZFirst = NatCoal + NZ_First)
seats$Total <- apply(seats[ , 1:9], 1, sum)


#==================presentation=====================

p <- seats %>%
  select(National, NatCoal, LabGreen, LabGreenNZFirst, NatCoalNZFirst) %>%
  gather(Coalition, Seats) %>%
  ggplot(aes(x = Seats, colour = Coalition, fill = Coalition)) +
  geom_density(alpha = 0.5)  +
  scale_y_continuous(limits = c(0, 0.2)) +
  ggtitle("Likely seat counts for various combinations of parties",
          "Most likely outcome is that New Zealand First are needed to build a majority.") +
  labs(caption = "Source: https://ellisp.github.io",
       y = "Likelihood")

svg("./output/gam-results-density.svg", 9, 4)
print(direct.label(p, "top.bumpup"))
dev.off()



svg("./output/gam-results-pairs.svg", 8, 7)
par(family = thefont, bty = "n", font.main = 1)
seats %>%
  mutate(Other = as.ordered(ACT + `United Future` + Conservative + Mana + Maori)) %>%
  dplyr::select(Green, Labour, National, NZ_First, Other) %>%
  pairs(diag.panel = panel.hist, upper.panel = panel.cor,
        main = paste("Possible outcomes for number of seats on", format(as.Date(ThisElection), "%d %B %Y")))
dev.off()

# space in `National ` is important as otherwise it gets confused with the original seat counts
chances <- seats %>%
  summarise(`National ` = mean(National > Total / 2),
            `National-led coalition similar to 2014` = mean(NatCoal > Total / 2 & National <= Total / 2),
            `Labour + Green` = mean(LabGreen > Total / 2),
            `Labour + Green + Mana` = mean(LabGreen + Mana > Total / 2 & LabGreen <= Total / 2),
            `NZ First needed to make government` = 
              mean((Green + Labour + Mana + NZ_First) >= Total / 2) - `Labour + Green + Mana`)
  

svg("./output/gam-final-chances-bar.svg", 8, 3)
print(chances %>%
  gather(outcome, prob) %>%
  mutate(outcome = factor(outcome, levels = names(chances)[c(1,2,5,4,3)])) %>%
  ggplot(aes(x = outcome, weight = prob, fill = outcome)) +
  geom_bar() +
  geom_text(aes(label = paste0(round(prob * 100, 1), "%"), y = prob +.039), colour = "darkred") +
  coord_flip() +
  scale_y_continuous("Chance of happening", label = percent, limits = c(0, 1)) +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE, option = "C", begin = 0.1, end = 0.9) +
  labs(x = "", caption = "Source: https://ellisp.github.io") +
  ggtitle("Probability of different outcomes for the New Zealand 2017 General Election",
          paste("Modelling based on polls from 2014 election to", format(Sys.Date(), "%d %B %Y")))
)
dev.off()

svg("./output/gam-final-chances-histogram.svg", 8, 4)
seats %>%
  gather(Party, Number) %>%
  mutate(Party = gsub("_", " ", Party)) %>%
  mutate(Party = fct_reorder(Party, Number)) %>%
  ggplot(aes(x = Number, y = ..density..)) +
  facet_wrap(~Party, scales = "free", ncol = 5) +
  geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.5, colour = "steelblue") +
  labs(x = "Number of seats", y = "Probability",
       caption = "http://ellisp.github.io") +
  ggtitle("Simulated election outcomes, 2017",
          "Forecasts based on opinion poll trends, calibrated to previous election outcomes")
dev.off()