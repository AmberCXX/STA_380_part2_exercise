library(mosaic)
library(quantmod)
library(foreach) 

# Technology Equity ETF:  FXL	First Trust Technology AlphaDEX Fund
mystocks = c("AMZN", "V", "FXL","BBY")
myprices = getSymbols(mystocks, from = "2015-01-01")

#adjusting all stocks
for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

head(AMZNa)

# Combine all the returns in a matrix
all_returns = cbind(ClCl(AMZNa),ClCl(Va),ClCl(FXLa),ClCl(BBYa))
head(all_returns)

all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
pairs(all_returns)

# simulate many different possible futures
initial_wealth = 100000
sim1 = foreach(i=1:50000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.25, 0.25, 0.25, 0.25)
  holdings = weights * total_wealth
  n_days = 20 #4 weeks
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}
head(sim1)
hist(sim1[,n_days], 80) #80 bins
# I have some great outliner...
range(sim1[,n_days])

# Profit/loss
mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=100)

# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
#5% of my simulated futures have a loss of worse than $8292.279
#95% of the simulated futures are better than losing $8292.279

##################################################################################################################
## second Diversified across many markets
##################################################################################################################
#Agricultural Commodities ETFs -FUD : E-TRACS UBS Bloomberg CMCI Food ETN
#Money Market IBMI: iShares iBonds Sep 2020 AMT-Free Muni Bond ETF
#Global Real Estate ETFs -GQRE: FlexShares Global Quality Real Estate Index Fund
#Global Equities ETFs -INKM: SPDR SSgA Income Allocation ETF
##################################################################################################################

mystocks = c("FUD", "IBMI","GQRE","INKM")
myprices = getSymbols(mystocks, from = "2015-01-01")

#adjusting all stocks
for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

head(FUDa)

# Combine all the returns in a matrix
all_returns = cbind( ClCl(FUDa),
                     ClCl(IBMIa),
                     ClCl(GQREa),
                     ClCl(INKMa)
                    )
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
pairs(all_returns)

# simulate many different possible futures
initial_wealth = 100000
sim1 = foreach(i=1:50000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.25, 0.25, 0.25, 0.25)
  holdings = weights * total_wealth
  n_days = 20 #4 weeks
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

head(sim1)
hist(sim1[,n_days], 25) #25 bins

# Profit/loss
mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=30)

# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
#5% of my simulated futures have a loss of worse than $4352.73 
#95% of the simulated futures are better than losing $4352.73 


##################################################################################################################
## Third A typical portfolio with bond, metal, and largeCaps 
##################################################################################################################
#RJZ: RICI-Metals ETN
#SVXY:	ProShares Short VIX Short-Term Futures
#SPVM:	Invesco S&P 500 Value with Momentum ETF
#IGM:	iShares Expanded Tech Sector ETF
#VBK:	Vanguard Small Cap Growth ETF
##################################################################################################################

mystocks = c("RJZ", "SVXY","SPVM","IGM","VBK")
myprices = getSymbols(mystocks, from = "2015-01-01")

#adjusting all stocks
for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

head(FUDa)

# Combine all the returns in a matrix
all_returns = cbind( ClCl(RJZa),
                     ClCl(SVXYa),
                     ClCl(SPVMa),
                     ClCl(IGMa),
                     ClCl(VBKa)
                     )
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
pairs(all_returns)

# simulate many different possible futures
initial_wealth = 100000
sim1 = foreach(i=1:50000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.2,0.2,0.2,0.2,0.2)
  holdings = weights * total_wealth
  n_days = 20 #4 weeks
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

head(sim1)
hist(sim1[,n_days], 25) #25 bins

# Profit/loss
mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=30)

# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
#5% of my simulated futures have a loss of worse than $-11462.21 
#95% of the simulated futures are better than losing $-11462.21
