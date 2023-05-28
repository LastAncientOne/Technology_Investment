library(quantmod)
library(PerformanceAnalytics)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Fetch stock data
getSymbols(symbols, from = start_date, to = end_date)

# Fetch benchmark data (SPY)
benchmark <- getSymbols("SPY", from = start_date, to = end_date)
Rb <- dailyReturn(Cl(benchmark))

# Calculate ratios for each stock
ratios <- list()
for (symbol in symbols) {
  returns <- dailyReturn(Cl(get(symbol)))
  ratios[[symbol]] <- table.AnnualizedReturns(returns, Rf = 0, scale = 252)  # Adjust scale as per your requirement
}

# Print the ratios for each stock
for (symbol in symbols) {
  print(symbol)
  print(ratios[[symbol]])
  cat("\n")
}