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

# Calculate the Sterling Ratio for each stock
sterling_ratios <- data.frame()
for (symbol in symbols) {
  sterling_ratio <- SterlingRatio(returns[[symbol]], excess = 0.1)
  sterling_ratios <- rbind(sterling_ratios, data.frame(Symbol = symbol, sterling_Ratio = sterling_ratio))
}

# Print the table of Sterling Ratio
print(sterling_ratios)
