

# good discussion here on iterations and chains https://groups.google.com/forum/#!topic/stan-users/5WG51xKNSbA
# The below is used on my 8 core machine.  For production chains=4, iter=1200
system.time({
  m1 <- stan(file = "method-statespace/ss-vectorized.stan", data = d1, 
             chains = 3, iter = 2000, control = list(max_treedepth = 20))
}) 
# with 1700 iterations per chain and max treedepth of 15 this takes about 13.5 hours  and still gets a warning that more 
# iterations are needed per chain in particlar 'The largest R-hat is NA, indicating chains have not mixed'
# From this we see a single NA
summary(summary(m1)$summary[,"Rhat"])
summary(summary(m1)$summary[,"n_eff"])


save(m1, file = glue("data/m1-{Sys.Date()}.rda"), compress = "xz")


# Warning messages:
#   1: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
# http://mc-stan.org/misc/warnings.html#bfmi-low 
# 2: Examine the pairs() plot to diagnose sampling problems
# 
# 3: The largest R-hat is NA, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#r-hat 
# 4: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#bulk-ess 
# 5: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#tail-ess 
# 6: `data_frame()` is deprecated as of tibble 1.1.0.
# Please use `tibble()` instead.