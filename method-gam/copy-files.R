# this is only for on Peter Ellis' own setup.  Copies output files over to his blog repository.
# Depends on simulations.R

dir("output")

files <- c("gam-final-chances-bar.svg",
           "gam-results-density.svg",
           "gam-results-pairs.png",
           "gam-vote-predictions-density.svg",
           "gam-vote-predictions.svg",
#           "gam-vote-predictions-2014.svg",
#           "gam-final-chances-bar-2014.svg",
           "gam-final-chances-histogram.svg")

file.copy(from = paste0("output/", files), 
          to = paste0("D:/Peter/Documents/blog/ellisp.github.io/img/", files),
          overwrite = TRUE)


projdir <- setwd("output/")
for(i in files[grepl("svg$", files)]){
  output <- gsub("svg$", "png", i)
  cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\"', " ", i, " ", output)
  system(cmd)
  
}

setwd(projdir)
