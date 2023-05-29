# Load required libraries
library(quantmod)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

# Define the symbols and dates
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Download historical stock prices
getSymbols(symbols, from = start_date, to = end_date)

# Extract closing prices
prices <- NULL
for (symbol in symbols) {
  prices <- cbind(prices, Cl(get(symbol)))
}
colnames(prices) <- symbols

# Calculate daily returns
returns <- diff(log(prices))

# Create a portfolio object
portfolio <- portfolio.spec(assets = symbols)

# Set the objective function for portfolio optimization
portfolio <- add.objective(portfolio, type = "risk", name = "var")

# Set the constraints for portfolio optimization
portfolio <- add.constraint(portfolio, type = "box", min = 0.01, max = 0.2)

# Set the solver for portfolio optimization
portfolio <- add.constraint(portfolio, type = "full_investment")

# Optimize the portfolio
result <- optimize.portfolio(returns, portfolio = portfolio, optimize_method = "ROI", trace = FALSE)

# Print the optimal weights
optimal_weights <- extractWeights(result)
print(optimal_weights)

# Sort Optimal Weights
sort(optimal_weights)

