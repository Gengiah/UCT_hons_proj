install.packages("knitr")
install.packages("gridExtra")
library(knitr)
library(gridExtra)
ss<- tableGrob(strat_all)
print(ss)


strat_all->smafin

 print(kable(strat_all))
 
 save(smafin, file= "smafin.Rdata")
 