# plotting

source('portfolio.R')

plot(cumprod(1+rlog_equity), col ='blue', lty=2)
lines(cumprod(1+rlog_bond), col = 'purple', lty = 5)
lines(cumprod(1+(0.6*rlog_equity+0.4*rlog_bond)), col = 'red', lty = 6)
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_bond)), col = 'black', lty = 6)

# levered portfolios
plot(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_bond)), col = 'blue', lty = 2, ylim=c(0,16))
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_bond2)), col = 'purple', lty = 3)
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_ebond)), col = 'red', lty = 4)
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_ebond2)), col = 'green', lty = 5)
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_gbond)), col = 'orange', lty = 6)
