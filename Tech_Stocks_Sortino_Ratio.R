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

# Calculate the Sortino Ratio for each stock
sortino_ratios <- data.frame()
for (symbol in symbols) {
  sortino_ratio <- SortinoRatio(returns[[symbol]], Rf = 0.01)
  sortino_ratios <- rbind(sortino_ratios, data.frame(Symbol = symbol, Sortino_Ratio = sortino_ratio))
}

# Print the Sortino ratios
print(sortino_ratios)