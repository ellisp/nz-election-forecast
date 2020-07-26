source("setup/functionality.R")

# Need this to be TRUE at least the first time you run the model, and whenever there is a new poll 
# available in nzelect
fit_model <- TRUE

ThisElection <- "2020-09-19"

source("method-statespace/prior.R")
source("method-statespace/prep.R")
#================state space model==================
# caution - takes more than 18 hours, has about 30,000+ parameters to estimate:

if(fit_model){
  system.time({source("method-statespace/fit.R")})  # about 80 minutes on 12 July 2017
  source("method-statespace/ss-diagnostics.R")
}

source("method-statespace/results.R")

source("setup/copy-files.R")






