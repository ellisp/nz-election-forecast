# this is only for on Peter Ellis' own setup.  Copies output files over to his blog repository.
# Depends on simulations.R

dir("output")

files <- c(paste0(rep(c("gam-", "state-space-", "combined-"), each = 6), 
                   c("final-chances-bar.svg",
                     "results-density.svg",
                     "results-pairs.png",
                     "vote-predictions-density.svg",
                     "final-chances-histogram.svg")),
           "gam-vote-predictions.svg",
           "state-space-ribbons.svg")

exists <- file.copy(from = paste0("output/", files), 
          to = paste0("D:/Peter/Documents/blog/ellisp.github.io/img/", files),
          overwrite = TRUE)

expect_equal(sum(!exists), 0)

projdir <- setwd("output/")
for(i in files[grepl("svg$", files[exists])]){
  output <- gsub("svg$", "png", i)
  cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\"', " ", i, " ", output)
  system(cmd)
  
}
setwd(projdir)
