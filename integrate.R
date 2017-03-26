# Install latest version of nzelect with most recent polling data
# (more up to date than the CRAN version)
# devtools::install_github("ellisp/nzelect/pkg1")

library(tidyverse)
library(magrittr)
library(forcats)
library(scales)
library(nzelect)
library(GGally)
library(boot) # for inv.logit
library(mgcv)
library(RColorBrewer)
library(extrafont)
library(directlabels)
library(viridis)

thefont <- "Calibri"

source("setup/set-fonts.R")
source("setup/functions.R")

# house effects, and variance of election results compared to predictions,
# using all available data
source("method-gam/estimate-house-effects.R")
source("method-gam/estimate-election-variance.R")

# Fit model and simulations for this current election year
source("method-gam/fit-gam.R")
source("method-gam/simulations.R")
write.csv(seats, file = "D:/Peter/Documents/blog/ellisp.github.io/elections/simulations.csv",
          row.names = FALSE)

# could turn the below into functions... or better, abstract the whole modelling process
# so it works for an arbitrary election, on an arbitrary reference data
source("method-gam/retro-2014/retro-integrate.R")
source("method-gam/retro-2014-1week/retro-integrate.R")

source("method-gam/copy-files.R")
