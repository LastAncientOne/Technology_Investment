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

# Calculate the Calmar Ratio for each stock
calmar_ratios <- data.frame()
for (symbol in symbols) {  
  calmar_ratio <- CalmarRatio(returns[[symbol]])  # Change 'ticker' to 'symbol'
  calmar_ratios <- rbind(calmar_ratios, data.frame(Symbol = symbol, Calmar_Ratio = calmar_ratio))
}

# Print the Calmar ratios
print(calmar_ratios)