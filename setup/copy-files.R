# this is only for on Peter Ellis' own setup.  Copies output files over to his blog repository.
# Depends on both the GAM and state space models having been run


# comment out one of the next two lines according to whether transferring over all
# the output from all three models, or just from the GAM Model A
files <- c(paste0(rep(c("gam-", "state-space-", "combined-"), each = 6), 
# files <- c(paste0(rep(c("gam-"), each = 6), 
                   c("final-chances-bar.svg",
                     "results-density.svg",
                     "results-pairs.png",
                     "vote-predictions-density.svg",
                     "final-chances-histogram.svg")),
           "gam-vote-predictions.svg", "gam-vote-predictions.png",
           "state-space-ribbons.svg", "election-forecast-tracking.svg")

projdir <- setwd("output/")
for(i in files[grepl("svg$", files)]){
  output <- gsub("svg$", "png", i)
  cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\"', " ", i, " ", output)
  system(cmd)
  
}
setwd(projdir)

# sleep for 10 seconds to make sure there is time for ImageMagick to finish its stuff
unlink("D:/Peter/Documents/blog/ellisp.github.io/img/gam-final-chances-bar.svg")
Sys.sleep(10)


exists <- file.copy(from = paste0("output/", files), 
          to = paste0("D:/Peter/Documents/blog/ellisp.github.io/img/", files),
          overwrite = TRUE)

expect_equal(sum(!exists), 0)

