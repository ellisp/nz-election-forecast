
# Depends on fit-gam.R

#==========simulations============
set.seed(123)
n <- 10000


# estimated cov matrix from GAM.  
mod_cov <- solve(crossprod(mod$family$data$R)) 

# estimated standard error for the predicted values.  This bit is uncontroversial.
se <- as.vector(mod_pred_elect[["se.fit"]])

# In my first iterations I was using sigma1 as the covariance matrix for 
# simulations.  But now I think this is understating the true randomness we should expect
# sigma1 <- se %*% t(se) * cov2cor(mod_cov)

# This second method takes the covariance of the residuals (mod_cov) and
# adds to it the uncertainty from the prediction.  It ends up being too *much*
# variance, because the election is treated as just another small sample poll
# sigma2 <- mod_cov 
#diag(sigma2) <- diag(sigma2) + sqrt(se)

# The third method takes the observed mean squared error from previous elections compared to
# where a gam fits the lines (see estimate-election-variance.R) and treats that as the 
# individual variance of the actual election observation.  This is added to the sum of the
# standard errors for the prediction of the latent party vote variable to create an expected
# standard deviation of the distribution for each party mean.  The correlation matrix
# from the GAM predicting this year's eleciton is used as a basis, scaled up by this standard
# deviation, to create a new covariance matrix
se3 <- as.vector(sqrt(se ^ 2 + 
                        pmin(exp(coef(mod_var)[1] + coef(mod_var)[2] * mod_pred_elect[["fit"]]), 1)))


# Now I want to add some uncertainty from the fact that house effects were only estimated.
house_effects_vars <- data_frame(Party = parties) %>%
  # add in parties without enough track record to have a house effect estimate:
  left_join(house_effects, by = "Party") %>%
  # replace NAs with the maximum observed house effect:
  mutate(SE = ifelse(is.na(SE), max(SE, na.rm = TRUE), SE)) %>%
  group_by(Party) %>%
  summarise(SE = sqrt(mean(SE ^ 2))) %>%
  # next step is to force the ordering to be the same as in `parties`
  right_join(data_frame(Party = parties))


se4 <- sqrt(se3 ^ 2 + house_effects_vars$SE ^ 2)

sigma3 <- se4 %*% t(se4) * cov2cor(mod_cov)

# round(sigma1, 2) # way too small
# round(sigma2, 2) # too big
# round(sigma3, 3) # generally (not always) in between
sims_gam <- inv.logit(MASS::mvrnorm(n = n, 
                                mu = mod_pred_elect[["fit"]],
                                Sigma = sigma3)) %>%
  as_tibble()
names(sims_gam) <- gsub("M.ori", "Maori", parties)

simulate_seats(sims_gam, prefix = "gam")

