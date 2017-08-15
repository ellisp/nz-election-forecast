# draw chart of predictions to date
# added August 2017
# TODO - make tracking.csv automatically update (currently done by hand)

tracking <- read.csv("data/tracking.csv", check.names = FALSE, stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Date)) %>%
  gather(model, value, -Date) %>%
  mutate(model = factor(model, levels = c("Model B", "Combined", "Model A")),
         value = value / 100)

model_palette <- brewer.pal(7, "Blues")
names(model_palette)[c(3, 5, 7)] <- levels(tracking$model)

latest <- tracking %>%
  filter(Date == max(Date) & model == "Model A")


svg("output/election-forecast-tracking.svg", 7, 4)
print(
ggplot(tracking, aes(x = Date, y = value, colour = model)) +
  geom_path() +
  geom_point() +
  theme(legend.position = "right") +
  scale_colour_manual(values = model_palette) +
  scale_y_continuous("Probability of National or National-led coalition\nwinning the 23 September 2017 election", label = percent) +
  labs(x = "Date of forecast", colour = "",
       caption = "Peter's Stats Stuff; https://ellisp.github.io") +
  ggtitle("2017 election forecasts changing over time",
          "Forecasts from two models and a combination of them") +
  annotate("text", x = as.Date("2017-06-01"), y = 0.5, 
           label = paste0(latest$value * 100, "% chance for\nNational or coalition"),
           colour = model_palette["Model A"],
           size = 6) +
  annotate("text", x = as.Date("2017-06-01"), y = 0.43,
           label = paste("as at", Sys.Date()),
           colour = model_palette["Model A"],
           size = 4)
)
dev.off()