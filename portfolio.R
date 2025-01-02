
# Mise en place
rm(list=ls()) # empty environment
setwd(getwd()) # set working directory
library(tidyquant) # getSymbols()

# download data and visualize
getSymbols('QQQ'); plot(QQQ$QQQ.Close) # Nasdaq 100
getSymbols('VCLT'); plot(VCLT$VCLT.Close) # Long-term corporate bonds
getSymbols('WIAU.L');plot(WIAU.L$WIAU.L.Close) # Non-investment grade bonds

