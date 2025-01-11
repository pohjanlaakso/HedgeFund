
source('portfolio.R')

# fetch data, rename columns, and plot
commodities <- getSymbols('GD=F', auto.assign = F)
colnames(commodities) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
plot(commodities$Close)

# 
r_commodities <- commodities$Close / lag(commodities$Close, k=1)-1
r_commodities <- na.omit(r_commodities)
plot(r_commodities)
