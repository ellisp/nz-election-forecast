
head(nzge)


incumbent_pm <- tribble(~ElectionYear, ~incumbent,
                        1996, "National",
                        1999, "National",
                        2002, "Labour",
                        2005, "Labour",
                        2008, "Labour",
                        2011, "National",
                        2014, "National",
                        2017, "National")

swing_99 <- tribble(~Party, ~swing, ~ElectionYear,
                    "National", -0.018, 1996,
                     "National", -.0337, 1999,
                    "Labour", .0252, 2002)  
  
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

d

model <- lm(swing ~ 1, data = d)
summary(model)

# mean swing: -0.01306, 
# standard error: 0.01123
# residual sd: 0.03175

sd <- summary(model)$sigma 
se <- summary(model)$coefficients[2]

mu_prior = as.numeric(coef(model))
sigma_prior <- sqrt(sd ^ 2 + se ^ 2)


ggplot(d, aes(x = swing)) +
  geom_rug(aes(colour = incumbent), size = 2) +
  geom_density(fill = "grey50", alpha = 0.5) +
  scale_x_continuous(label = percent) +
  labs(title = "Swings to the government in New Zealand since the introduction of MMP",
       subtitle = glue("Mean swing of {percent(mu_prior, accuracy = 0.1)}, standard deviation of {round(sigma_prior * 100,1)} percentage points."))

