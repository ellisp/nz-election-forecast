# Install latest version of nzelect with most recent polling data
# (more up to date than the CRAN version)
devtools::install_github("ellisp/nzelect/pkg1")
library(Cairo)
library(tidyverse)
library(magrittr)
library(forcats)
library(scales)
library(nzelect)
library(GGally)
library(grid)
library(boot) # for inv.logit
library(mgcv)
library(RColorBrewer)
library(extrafont)
library(directlabels)
library(viridis)
library(rsconnect)
library(rstan)
library(testthat)

thefont <- "Calibri"

source("setup/set-fonts.R")
source("setup/functions.R")
source("setup/simulate-seats.R")

rstan_options(auto_write = TRUE)
options(mc.cores = 7)


ThisElection <- "2017-09-23"

electionday <- data_frame(
  MidDate = as.numeric(as.Date(ThisElection))
)



#=============GAM model=========================
# compile Stan function used for regularized house effects, if hasn't already been done
# disc <- rnorm(10)
# x <- list(disc = disc, N = length(disc))
# fit <- stan(file = 'method-gam/estimate-house-effects.stan', data = x, control = list(adapt_delta = 0.995))


# house effects, and variance of election results compared to predictions,
# using all available data
source("method-gam/estimate-house-effects.R") # note - calls Stan separately for each party - pollster combo, could be refactored!
source("method-gam/estimate-election-variance.R")

# Fit model and simulations for this current election year
source("method-gam/fit-gam.R")
source("method-gam/simulations.R")

# next line is specific to my (PE) setup, saving a copy of the seat simulations for my webpage:
write.csv(seats_gam, file = "D:/Peter/Documents/blog/ellisp.github.io/elections/simulations.csv",
          row.names = FALSE)

#================state space model==================
# caution - takes more than an hour, has about 20,000 parameters to estimate
# still needs some more refactoring!:
system.time({source("method-statespace/ss-main.R")})  # about 80 minutes on 12 July 2017
# (fastest version at 20 minutes required assuming iid innovations in state space)

#================combined================
# this takes simulations from the state space model, and
# the same number from the GAM model, and combines the two
equal_rows <- min(min(nrow(sims_gam), nrow(sims_ss)), 2000)

sims_combined <- rbind(sims_gam[1:equal_rows, ], sims_ss[1:equal_rows, ])
simulate_seats(sims_combined, prefix = "combined")

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


