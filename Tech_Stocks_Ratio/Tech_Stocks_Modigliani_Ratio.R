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

# Calculate the Modigliani-Modigliani Ratio for each stock
modigliani_ratios <- data.frame()
for (symbol in symbols) {
  modigliani_ratio <- Modigliani(returns[[symbol]], Rb, Rf = 0.01)
  modigliani_ratios <- rbind(modigliani_ratios, data.frame(Symbol = symbol, Modigliani_Ratio = modigliani_ratio))
}

# Print the table of Modigliani-Modigliani Ratios
print(modigliani_ratios)