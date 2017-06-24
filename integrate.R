# Install latest version of nzelect with most recent polling data
# (more up to date than the CRAN version)
devtools::install_github("ellisp/nzelect/pkg1")

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

thefont <- "Calibri"

source("setup/set-fonts.R")
source("setup/functions.R")
source("setup/simulate-seats.R")

rstan_options(auto_write = TRUE)
options(mc.cores = 7)


#=============GAM model=========================
# compile Stan function used, if hasn't already been done
# disc <- rnorm(10)
# x <- list(disc = disc, N = length(disc))
# fit <- stan(file = 'method-gam/estimate-house-effects.stan', data = x, control = list(adapt_delta = 0.995))


# house effects, and variance of election results compared to predictions,
# using all available data
source("method-gam/estimate-house-effects.R")
source("method-gam/estimate-election-variance.R")

# Fit model and simulations for this current election year
source("method-gam/fit-gam.R")
source("method-gam/simulations.R")

# next line is specific to my (PE) setup, saving a copy of the seat simulations for my webpage:
write.csv(seats, file = "D:/Peter/Documents/blog/ellisp.github.io/elections/simulations.csv",
          row.names = FALSE)

#================state space model==================
# caution - takes about 6 hours, has about 20,000 parameters to estimate
system.time({source("method-statespace/ss-main.R")})


#================combined================
# this takes 1000 simulations from the state space model, and
# the same number from the GAM model, and combines the two
equal_rows <- min(nrow(sims_gam), nrow(sims_ss))

sims_combined <- rbind(sims_gam[1:equal_rows, ], sims_ss[1:equal_rows, ])
simulate_seats(sims_combined, prefix = "combined")








#=======for shiny app========
# set which shiny app to deploy: nz-election-2017 for produ, nz-election-2017-test for testing
app_name <- "nz-election-2017-test"
# app_name <- "nz-election-2017"
source("method-gam/shiny-prep.R")

# could turn the below into functions... or better, abstract the whole modelling process
# so it works for an arbitrary election, on an arbitrary reference data
# source("method-gam/retro-2014/retro-integrate.R")
# source("method-gam/retro-2014-1week/retro-integrate.R")

source("method-gam/copy-files.R")
