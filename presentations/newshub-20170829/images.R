library(tidyverse)
library(scales)
library(nzelect)
library(extrafont)
library(forcats)
library(RColorBrewer)
library(viridis)

try( setwd("presentations/newshub-20170829"))

theme_set(theme_light(base_family = "Calibri") +
            theme(plot.caption = element_text(colour = "grey50"))) 

pollsters <- unique(polls$Pollster)
pollsters <- pollsters[!grepl("Election result", Pollsters)]

brewer.pal(13, "Paired")
rainbow(14)
palette <- c("black", hue_pal()(13))
names(palette) <- c("Election result", pollsters)

p <- polls %>%
  filter(Party %in% c("Labour", "National", "NZ First", "Green")) %>%
  mutate(Party = fct_reorder(Party, -VotingIntention)) %>%
  ggplot(aes(x = MidDate, y = VotingIntention, colour = Pollster)) +
  facet_wrap(~Party, scales = "free_y") +
  geom_line() +
  geom_text(aes(label = ifelse(Pollster == "Election result", "O", "")), 
            size = 8, colour = "black") +
  scale_y_continuous(label = percent) +
  labs(x = "Polling date",
       y = "Voting intention",
       caption = "Peter's Stats Stuff, http://ellisp.github.io") +
  scale_colour_manual(values = palette)

svg("all-polls-and-years.svg", 8, 5)
print(p)
dev.off()
