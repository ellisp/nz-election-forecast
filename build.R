source("setup/functionality.R")

fit_model <- FALSE

ThisElection <- "2020-09-19"

electionday <- tibble(
  MidDate = as.numeric(as.Date(ThisElection))
)

source("method-statespace/prior.R")
source("method-statespace/prep.R")
#================state space model==================
# caution - takes more than 18 hours, has about 30,000+ parameters to estimate:

if(fit_model){
  system.time({source("method-statespace/fit.R")})  # about 80 minutes on 12 July 2017
}

source("method-statespace/ss-diagnostics.R")
source("method-statespace/results.R")

source("setup/copy-files.R")

#================combined================

# # manual edit needed at this point before re-creating the tracking plot
# source("tracking-plot.R")
# 
# #=======for shiny app and final distribution========
# # which simulated party vote to use for shiny app?
# sims <- sims_combined %>%
#   mutate(model = rep(c("Model A", "Model B"), each = equal_rows))
# 
# # set which shiny app to deploy: nz-election-2017 for produ, nz-election-2017-test for testing
# app_name <- "nz-election-2017-test"
# # app_name <- "nz-election-2017"
# source("setup/shiny-prep.R")
# 
# 
# 
# 




