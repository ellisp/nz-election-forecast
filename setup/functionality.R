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




rstan_options(auto_write = TRUE)
options(mc.cores = 7)

myfont <- "Roboto"
main_font <- "Roboto"
heading_font <- "Sarala"
thefont <- "Roboto"

theme_set(theme_light(base_family = main_font) + 
            theme(legend.position = "bottom") +
            theme(plot.caption = element_text(colour = "grey50"),
                  strip.text = element_text(size = rel(1), face = "bold"),
                  plot.title = element_text(family = heading_font))
) 
update_geom_defaults("text", list(family = main_font))
update_geom_defaults("label", list(family = main_font))


source("setup/set-fonts.R")
source("setup/functions.R")
source("setup/simulate-seats.R")