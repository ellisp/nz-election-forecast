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

thefont <- "Calibri"

source("setup/set-fonts.R")


source("method-gam/estimate-house-effects.R")
source("method-gam/fit-gam.R")
source("method-gam/simulations.R")