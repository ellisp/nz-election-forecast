# "Election eve" point and range predictions
# 23 September 2017

library(tidyverse)
load("shiny/sims.rda")

# Combined point estimate
sims %>% 
  select(-model) %>%
  gather(party, value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(party) %>%
  summarise(value = round(mean(value) * 100, 1)) %>%
  filter(value > 0) %>%
  arrange(desc(value))

# Range
sims %>% 
  gather(party, value, -model) %>%
  mutate(value = 100 * as.numeric(value)) %>%
  group_by(party, model) %>%
  summarise(low = floor(quantile(value, 0.025)),
            high = ceiling(quantile(value, 0.975))) %>%
  group_by(party) %>%
  summarise(low = min(low),
            high = max(high)) %>%
  arrange(desc(high)) %>%
  mutate(text = paste(party, low, "-", high)) %>%
  select(text)

# Model A compared to model B
sims %>% 
  gather(party, value, -model) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(party, model) %>%
  summarise(value = round(mean(value) * 100, 1)) %>%
  filter(value > 0) %>%
  spread(model, value) %>%
  arrange(desc(`Model B`))
  
