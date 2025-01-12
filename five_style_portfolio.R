# Based on Antti Ilmanen's value, momentum, carry, defensive, and trend-following
# Value: buy when undervalued relative to historical norms and sell when overvalued --> CAPE-ratio
# Momentum:  If 12-month return is +10%, stay invested or overweight. If the return turns negative (e.g., -5%), reduce exposure
# Carry: If the market's dividend yield is 3 % while bond yields are 1.5 %, overweight the market. If bond yields rise to 4%, reduce allocation.
# Defensive: If the VIX [a forward looking measure] rises above 30 (indicating market stress), reduce market exposure. When VIX falls below 15, increase exposure.
# Trend:  If the market's price is above its 200-day moving average, remain invested. If it drops below, exit the position. 

# Multi-Factor Approach: Use a combination of these strategies to create a composite signal for SPY.
# Example weighting: Value: 20%, Momentum: 20%, Carry: 20%, Defensive: 20%, Trend-Following: 20%
# Create a scoring system where SPY is overweighted when multiple factors are favorable (e.g., undervalued, positive momentum, and strong carry) and underweighted when unfavorable.

# economic policy uncertainty index: https://www.policyuncertainty.com/trade_uncertainty.html
# geopolitical risk index: https://www.matteoiacoviello.com/gpr.htm 
# maybe these two should play into the defensive strategy as well?

source('Portfolio.R')

