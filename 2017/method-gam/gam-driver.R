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

