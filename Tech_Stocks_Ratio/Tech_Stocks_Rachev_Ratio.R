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

# Calculate the Rachev Ratio for each stock
rachev_ratios <- data.frame()
for (symbol in symbols) {
  rachev_ratio <- RachevRatio(returns[[symbol]])
  rachev_ratios <- rbind(rachev_ratios, data.frame(Symbol = symbol, Rachev_Ratio = rachev_ratio))
}

# Print the table of Rachev Ratio
print(rachev_ratios)