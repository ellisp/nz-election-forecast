source("setup/functionality.R")


ThisElection <- "2020-09-19"

electionday <- tibble(
  MidDate = as.numeric(as.Date(ThisElection))
)

source("method-statespace/prior.R")

#================state space model==================
# caution - takes more than an hour, has about 20,000 parameters to estimate:
system.time({source("method-statespace/prep-and-fit.R")})  # about 80 minutes on 12 July 2017

source("method-statespace/ss-diagnostics.R")
source("method-statespace/results.R")

#================combined================

# manual edit needed at this point before re-creating the tracking plot
source("tracking-plot.R")

#=======for shiny app and final distribution========
# which simulated party vote to use for shiny app?
sims <- sims_combined %>%
  mutate(model = rep(c("Model A", "Model B"), each = equal_rows))

source("setup/copy-files.R")

# set which shiny app to deploy: nz-election-2017 for produ, nz-election-2017-test for testing
app_name <- "nz-election-2017-test"
# app_name <- "nz-election-2017"
source("setup/shiny-prep.R")







# could turn the below into functions... or better, abstract the whole modelling process
# so it works for an arbitrary election, on an arbitrary reference data
# source("method-gam/retro-2014/retro-integrate.R")
# source("method-gam/retro-2014-1week/retro-integrate.R")


