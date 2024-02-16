library(quantmod)
library(PerformanceAnalytics)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

getSymbols(symbols, from = start_date, to = end_date, adjust = TRUE)

# Get the Benchmark data
getSymbols("SPY", from = start_date, to = end_date)
Rb <- dailyReturn(Ad(merge(SPY)))

# Calculate the daily returns for each stock
returns <- list()
for (symbol in symbols) {
  returns[[symbol]] <- dailyReturn(Ad(get(symbol)))
}

# Calculate the Treynor Ratio for each stock
treynor_ratios <- data.frame()
for (symbol in symbols) {
  treynor_ratio <- TreynorRatio(returns[[symbol]], Rb, Rf = 0.01)
  treynor_ratios <- rbind(treynor_ratios, data.frame(Symbol = symbol, Treynor_ratio = treynor_ratio))
}

# Print the table of Treynor Ratios
print(treynor_ratios)