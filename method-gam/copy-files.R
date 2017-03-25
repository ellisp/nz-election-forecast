# this is only for on Peter Ellis' own setup.  Copies output files over to his blog repository.
# Depends on simulations.R

dir("output")

files <- c("gam-final-chances-bar.svg",
           "gam-results-density.svg",
           "gam-results-pairs.svg",
           "gam-vote-predictions-density.svg",
           "gam-vote-predictions.svg")

file.copy(from = paste0("output/", files), 
          to = paste0("D:/Peter/Documents/blog/ellisp.github.io/img/", files))

write.csv(seats, file = "D:/Peter/Documents/blog/ellisp.github.io/elections/simulations.csv",
          row.names = FALSE)
