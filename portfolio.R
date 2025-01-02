
# Mise en place
rm(list=ls()) # empty environment
setwd(getwd()) # set working directory
library(tidyquant) # getSymbols()

# download data and visualize
getSymbols('QQQ'); plot(QQQ$QQQ.Close) # Nasdaq 100
getSymbols('VCLT'); plot(VCLT$VCLT.Close) # Long-term corporate bonds
getSymbols('FALN');plot(FALN$FALN.Close) # Non-investment grade bonds

# basic return: https://www.r-bloggers.com/2020/05/basic-statistical-concepts-for-finance/#google_vignette
r_equity <- QQQ$QQQ.Close / lag(QQQ$QQQ.Close, k=1)-1; r_equity <- na.omit(r_equity); plot(r_equity)
r_bond <- VCLT$VCLT.Close / lag(VCLT$VCLT.Close, k=1)-1; r_bond <- na.omit(r_bond); plot(r_bond)
r_bond2 <- FALN$FALN.Close / lag(FALN$FALN.Close, k=1)-1; r_bond2 <- na.omit(r_bond2); plot(r_bond2)

# logarithmic return
rlog_equity <- log(QQQ$QQQ.Close) - log(lag(QQQ$QQQ.Close, k=1)); rlog_equity <- na.omit(rlog_equity); plot(rlog_equity)
rlog_bond <- log(VCLT$VCLT.Close) - log(lag(VCLT$VCLT.Close, k=1)); rlog_bond <- na.omit(rlog_bond); plot(rlog_bond)
rlog_bond2 <- log(FALN$FALN.Close) - log(lag(FALN$FALN.Close, k=1)); rlog_bond2 <- na.omit(rlog_bond2); plot(rlog_bond2)



