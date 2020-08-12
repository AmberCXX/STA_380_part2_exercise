library(mosaic)
library(quantmod)
library(foreach) 


mystocks = c("AMZN", "V", "MRNA","BBY")
myprices = getSymbols(mystocks, from = "2015-01-01")

#adjusting all stocks
for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

head(AMZNa)

# Combine all the returns in a matrix
all_returns = cbind(	ClCl(AMZNa),
                     ClCl(Va),
                     ClCl(MRNAa),
                     ClCl(BBYa)
                    )
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
pairs(all_returns)

# Sample a random return
return.today = resample(all_returns, 1, orig.ids=FALSE)

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
#5% of my simulated futures have a loss of worse than $10119
#95% of the simulated futures are better than losing $10119

#########################################################
## second
#########################################################
mystocks = c("AAPL", "GS")
myprices = getSymbols(mystocks, from = "2015-01-01")

#adjusting all stocks
for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

head(AMZNa)

# Combine all the returns in a matrix
all_returns = cbind(	ClCl(AAPLa),
                     ClCl(GSa)
                     
)
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
pairs(all_returns)

# Sample a random return
return.today = resample(all_returns, 1, orig.ids=FALSE)

# simulate many different possible futures
initial_wealth = 100000
sim1 = foreach(i=1:50000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.5,0.5)
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
#5% of my simulated futures have a loss of worse than $10588
#95% of the simulated futures are better than losing $10588