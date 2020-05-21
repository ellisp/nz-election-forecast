
# create some data as though we were six months before the 2014 election

polls_retro <- polls %>%
  filter(EndDate < as.Date("2014-03-20") & !is.na(VotingIntention))


