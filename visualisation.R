# plotting

source('portfolio.R')

plot(cumprod(1+rlog_equity), col ='blue', lty=2, ylim = c(0,13))
lines(cumprod(1+rlog_bond), col = 'purple', lty = 5)
lines(cumprod(1+(0.6*rlog_equity+0.4*rlog_bond)), col = 'red', lty = 6)
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_bond)), col = 'black', lty = 6)
lines(cumprod(1+(0.6*rlog_equity+0.4*rlog_fallen)), col = 'yellow', lty = 7)
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_fallen)), col = 'green', lty = 7)
lines(cumprod(1 + rlog_btc), col = 'cyan')
#legend('topleft', legend = c('a', 'b', 'c', 'd', 'e', 'f'), col = 1:6, lty = 1)

# levered portfolios
plot(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_bond)), col = 'blue', lty = 2, ylim=c(0,16))
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_bond2)), col = 'purple', lty = 3)
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_ebond)), col = 'red', lty = 4)
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_ebond2)), col = 'green', lty = 5)
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_gbond)), col = 'orange', lty = 6)
lines(cumprod(1+2*(0.6*rlog_equity+0.4*rlog_fallen)), col = 'black', lty = 7)

fallen_6040 <- 0.6 * returns_aligned$QQQ.Close + 0.4 * returns_aligned$ANGL.Close
vclt_6040 <- 0.6 * returns_aligned$QQQ.Close + 0.4 * returns_aligned$VCLT.Close
ebond_6040 <- 0.6 * returns_aligned$QQQ.Close + 0.2 * returns_aligned$IEAC.AS.Close + 0.2 *returns_aligned$ANGL.Close
btc_nasdaq <- 0.02 * returns_aligned$BTC.USD.Close + 0.58 * returns_aligned$QQQ.Close + 0.4 * returns_aligned$IEAC.AS.Close

plot(cumprod(1 + returns_aligned$QQQ.Close),col = 'blue', lty = 2, ylim = c(0, 6), 
     main = "Cumulative Returns (Aligned)", ylab = "Growth of 1", xlab = "Date")
lines(cumprod(1 + returns_aligned$VCLT.Close), col = 'purple', lty = 5)
lines(cumprod(1 + fallen_6040), col = 'red', lty = 6)
lines(cumprod(1 + 1.8 * fallen_6040), col = 'black', lty = 6)
lines(cumprod(1 + vclt_6040), col = 'yellow', lty = 7)
lines(cumprod(1 + 1.8 * vclt_6040), col = 'green', lty = 7)
lines(cumprod(1 + ebond_6040), col = 'pink')
lines(cumprod(1 + 1.8 * ebond_6040), col = 'cyan')
lines(cumprod(1 + 1.8 * btc_nasdaq), col = 'grey')

(mean(returns_aligned$QQQ.Close)/sd(returns_aligned$QQQ.Close))*252

mean(returns_aligned$QQQ.Close) / sd(returns_aligned$QQQ.Close)
mean(returns_aligned$VCLT.Close) / sd(returns_aligned$VCLT.Close)
mean(fallen_6040) / sd(fallen_6040)
mean(vclt_6040) / sd(vclt_6040)

plot(cumprod(1 + returns_aligned$VCLT.Close))
lines(cumprod(1 + returns_aligned$ANGL.Close), col = 2)

cor(returns_aligned)
