# Estimate mean and standard deviation of swings to/against government, for using
# as a prior in the main model
#
# Peter Ellis 30 May 2020

#-----------------prepare data on swings in election years-------------------
# some of this we can get from our 'polls' object in nzelect, which has the restults back
# to 2005

incumbent_pm <- tribble(~ElectionYear, ~incumbent,
                        1996, "National",
                        1999, "National",
                        2002, "Labour",
                        2005, "Labour",
                        2008, "Labour",
                        2011, "National",
                        2014, "National",
                        2017, "National")

# the others we hard code, based on manual inspection of Wikipedia:
swing_99 <- tribble(~Party, ~swing, ~ElectionYear,
                    "National", -0.018, 1996,
                     "National", -.0337, 1999,
                    "Labour", .0252, 2002)  

# combine into a single data frmae"  
d <- polls %>%
  as_tibble() %>%
  filter(Pollster == "Election result") %>%
  filter(Party %in% c("Labour", "National")) %>%
  arrange(MidDate) %>%
  group_by(Party) %>%
  mutate(swing = VotingIntention - lag(VotingIntention)) %>%
  select(Party, swing, ElectionYear) %>%
  ungroup() %>%
  drop_na() %>%
  rbind(swing_99) %>%
  arrange(ElectionYear, Party) %>%
  left_join(incumbent_pm, by = "ElectionYear") %>%
  filter(Party == incumbent)

# fit a model and get the mean and standard deviation"
model <- lm(swing ~ 1, data = d)
summary(model)

# mean swing: -0.01306, 
# standard error: 0.01123
# residual sd: 0.03175

sd <- summary(model)$sigma 
se <- summary(model)$coefficients[2]

mu_prior = as.numeric(coef(model))
sigma_prior <- sqrt(sd ^ 2 + se ^ 2) # take into account uncertainty in model as well as population variance


# visual check:
p2 <- d %>%
  ggplot(aes(x = ElectionYear, y = swing)) +
  geom_line() +
  geom_point(aes(colour = incumbent), size = 5) +
  scale_colour_manual(values = parties_v) +
  scale_y_continuous(label = percent) +
  labs(x = "Election Year",
       colour = "Party of incumbent Prime Minister",
       y = "Swing to government",
       title = "Swing to and against the government in New Zealand, 1996 to 2017",
       subtitle = glue("Mean of {percent(mu_prior, accuracy = 0.1)}, standard deviation of {percent(sigma_prior, accuracy = 0.1)}"),
       caption = "Source: https://freerangestats.info")

p2b <- function(){
  print(ggMarginal(p2, margins = "y", fill = "grey"))
  }

svg_png(p2b, "output/state-space-prior")
