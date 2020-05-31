

# good discussion here on iterations and chains https://groups.google.com/forum/#!topic/stan-users/5WG51xKNSbA
# The below is used on my 8 core machine.  For production chains=4, iter=1200
system.time({
  m1 <- stan(file = "method-statespace/ss-vectorized.stan", data = d1, 
             chains = 4, iter = 2500, control = list(max_treedepth = 20))
}) 
# with 1700 iterations per chain and max treedepth of 15 this takes about 13.5 hours  and still gets a warning that more 
# iterations are needed per chain in particlar 'The largest R-hat is NA, indicating chains have not mixed'
# From this we see a single NA
summary(summary(m1)$summary[,"Rhat"])
summary(summary(m1)$summary[,"n_eff"])


save(m1, file = glue("data/m1-{Sys.Date()}.rda"), compress = "xz")

