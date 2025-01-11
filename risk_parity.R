
source('portfolio.R')

# fetch commodities data, rename columns, and plot
commodities <- getSymbols('GD=F', auto.assign = F)
colnames(commodities) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
plot(commodities$Close)

# simple returns
r_commodities <- commodities$Close / lag(commodities$Close, k=1)-1
r_commodities <- na.omit(r_commodities)
plot(r_commodities)

# log returns
rlog_commodities <- log(commodities$Close) - log(lag(commodities$Close, k=1))
rlog_commodities <- na.omit(rlog_commodities)
plot(rlog_commodities)

# geometric mean return
geomean_commodities <- exp(mean(log(1+r_commodities)))^252-1
gmean_test(r_commodities, rlog_commodities)

# portfolio variance
