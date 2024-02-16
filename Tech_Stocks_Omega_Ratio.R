library(quantmod)
library(PerformanceAnalytics)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

getSymbols(symbols, from = start_date, to = end_date, adjust = TRUE)

# Calculate the daily returns for each stock
returns <- list()
for (symbol in symbols) {
  returns[[symbol]] <- dailyReturn(Ad(get(symbol)))
}

# Calculate the Omega Sharpe Ratio for each stock
omegasharpe_ratios <- data.frame()
for (symbol in symbols) {
  omegasharpe_ratio <- OmegaSharpeRatio(returns[[symbol]])
  omegasharpe_ratios <- rbind(omegasharpe_ratios, data.frame(Symbol = symbol, OmegaSharpe_Ratio = omegasharpe_ratio))
}

# Print the table of Omega Sharpe Ratio
print(omegasharpe_ratios)