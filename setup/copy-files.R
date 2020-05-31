# this is only for on Peter Ellis' own setup.  Copies output files over to his blog repository.


files <- list.files("output", pattern = "state-space", full.names = FALSE)

success <- file.copy(from = paste0("output/", files), 
          to = paste0("D:/Peter/Documents/blog/ellisp.github.io/img/nz-elections-2020/", files),
          overwrite = TRUE)

expect_equal(sum(!success), 0)

