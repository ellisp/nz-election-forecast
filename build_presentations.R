library(rmarkdown)
library(knitr)

projdir <- setwd("presentations/wrug-20170816")
render("wrug2017.Rmd")
setwd(projdir)



projdir <- setwd("presentations/newshub-20170829")
render("ellis-forecasting-elections.md", encoding = "UTF-8")

