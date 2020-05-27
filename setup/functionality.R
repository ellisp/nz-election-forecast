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
library(tidyverse)
library(scales)
library(nzelect)
library(forcats)
library(rstan)
library(directlabels)
library(lubridate)
library(frs)
library(glue)


thefont <- "Calibri"

rstan_options(auto_write = TRUE)
options(mc.cores = 7)

source("setup/set-fonts.R")
source("setup/functions.R")
source("setup/simulate-seats.R")