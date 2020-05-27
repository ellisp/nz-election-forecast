

ss_diagnostics <- function(m1){
  extracts <- extract(m1)
  
  CairoPDF("output/ss-diagnostics.pdf", 11, 8)
  print(ggpairs(as.data.frame(extracts$mu[ , sum(d1$n_days), ]), title = "Election day mu"))
  print(ggpairs(as.data.frame(extracts$epsilon[ , sum(d1$n_days), ]), title = "Election day epsilon"))
  
  print(ggpairs(as.data.frame(extracts$mu[ , 1000, ]), title = "Day 1000 mu"))
  print(ggpairs(as.data.frame(extracts$epsilon[ , 1000, ]), title = "Day 1000 epsilon"))
  
  print(ggpairs(as.data.frame(extracts$mu[ , 2000, ]), title = "Day 2000 mu"))
  print(ggpairs(as.data.frame(extracts$epsilon[ , 2000, ]), title = "Day 2000 epsilon"))
  
  
  dev.off()
  
}

ss_diagnostics(m1)