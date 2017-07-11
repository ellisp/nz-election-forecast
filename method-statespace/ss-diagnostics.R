
extracts <- extract(m1)

CairoPDF("output/ss-diagnostics.pdf", 11, 8)
print(ggpairs(as.data.frame(extracts$mu[ , 2130, ]), title = "Election day mu"))
print(ggpairs(as.data.frame(extracts$epsilon[ , 2130, ]), title = "Election day epsilon"))

print(ggpairs(as.data.frame(extracts$mu[ , 1000, ]), title = "Day 1000 mu"))
print(ggpairs(as.data.frame(extracts$epsilon[ , 1000, ]), title = "Day 1000 epsilon"))
dev.off()
