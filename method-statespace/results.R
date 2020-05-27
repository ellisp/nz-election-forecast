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
  ggtitle("Voting intention from the 2011 to the 2020 elections",
          paste("State-space modelling based on polls from 2011 to", format(Sys.Date(), "%d %B %Y"))) +  
  labs(caption = "https://freerangestats.io/elections/elections.html")

p1f <- function(){
  print(p1)
  grid.text(0.65, 0.2, label = "Vertical lines indicate elections.  Points show poll results.
Estimates of latent voting intention take into account past under 
and over-estimates of polling firms.",
            gp = gpar(fontfamily = thefont))
  
}

svg_png(p1f, "./output/state-space-ribbons", w = 10, h = 6)  

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
p2 <- data.frame(d = round(summary(m1, pars = "d")$summary[, "mean"] * 100, 2),
                 pollster = rep(pollsters, each = length(parties_ss)),
                 party = rep(parties_ss, length(pollsters))) %>%
  arrange(party) %>%
  ggplot(aes(y = party, colour = pollster, x = d, shape = pollster)) +
  geom_point(size = 2) +
  labs(x = "Average house effect (positive numbers mean the pollster over-estimates vote for that party)",
       y = "", colour = "", shape = "")

svg_png(p2, "./output/state-space-house-effects", w= 8, h= 6)


# extract the simulations for the final election day (and make up for the the three tiny parties)
sims_ss <- data.frame(rstan::extract(m1, "mu")$mu[ , sum(days_between_elections), ]) 
names(sims_ss)[1:length(parties_ss)] <- parties_ss
sims_ss <- select(sims_ss, -Other)

seats_ss <- simulate_seats(sims_ss, prefix = "state-space")



summary(m1, pars = "reid_impact")$summary %>%
  as_tibble() 

reid_impact <- rstan::extract(m1, "reid_impact") %>%
  as.data.frame() %>%
  as_tibble()

names(reid_impact) <- parties_ss

parties_v2 <- parties_v
names(parties_v2) <- gsub("M.ori", "Maori", names(parties_v))

p3 <- reid_impact %>%
    gather(party, value) %>%
    ggplot(aes(x = value, fill = party)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 0) +
    scale_fill_manual(values = parties_v2) +
    facet_wrap(~party) +
    theme(legend.position = "none") +
    ggtitle("Estimated impact of the change in Reid Research methodology in 2017") +
    scale_x_continuous("Increase in reported voting intention that can be attributed to the methodology change", 
                       label = percent)

svg_png(p3, "./output/reid-methodology-change-impact", 8, 6)
