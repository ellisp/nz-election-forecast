
# create some data as though we were six months before the 2014 election
LatestPoll <- "2014-09-13"
polls_retro <- polls %>%
  filter(EndDate < as.Date(LatestPoll) & !is.na(VotingIntention))


