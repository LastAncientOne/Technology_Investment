library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(tidyquant)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Fetch stock data
stock_prices  <- tq_get(symbols, get = "stock.prices", from = start_date, to = end_date)
# getSymbols(symbols, from = start_date, to = end_date)

stock_prices=stock_prices%>%select(symbol,date,adjusted)%>%rename(Stock=symbol,Price=adjusted) 
stock_prices%>%head()

#remove duplicated rows
stock_prices=stock_prices[!duplicated(stock_prices), ]
#Alternatively
#stock_prices[unique(stock_prices), ]
stock_prices2=stock_prices%>%spread(Stock,Price)
stock_prices2%>%head()

# Get stock pairs
stock_prices2 <- symbols %>%
  tq_get(get  = "stock.prices",
         from = "2016-01-01",
         to   = '2023-01-01') %>%
  group_by(symbol) 
stock_prices2%>%head()


stock_returns <- stock_prices2 %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               type       = "arithmetic",
               col_rename = "returns") %>%
  spread(key = symbol, value = returns)
stock_returns%>%head()

stock_prices2 %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               type       = "arithmetic",
               col_rename = "returns") %>%
  ggplot(aes(x = date, y = returns, fill = symbol)) +
  geom_bar(stat = "identity", width = 30) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = " Monthly Returns",
       subtitle = "",
       y = "Monthly Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2,scales = "free") +
  theme_tq() + 
  scale_fill_tq()

stock_prices_daily <- stock_prices2 %>%
  group_by(symbol)
stock_prices_daily %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()


stock_prices2 %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               type       = "arithmetic",
               col_rename = "returns")%>%
  ggplot(aes(x = date, y = returns, fill = symbol)) +
  geom_line(aes(color=symbol)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = " Monthly Returns",
       subtitle = "",
       y = "Monthly Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2,scales = "free") +
  theme_tq() + 
  scale_fill_tq()

stock_prices2 %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               type       = "arithmetic",
               col_rename = "returns")%>%
  ggplot(aes(x = date, y = returns, fill = symbol)) +
  geom_smooth(aes(color=symbol),method = 'loess' , formula = y ~ x) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = " Monthly Returns",
       subtitle = "",
       y = "Monthly Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2,scales = "free") +
  theme_tq() + 
  scale_fill_tq()

stock_prices2 %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               type       = "arithmetic",
               col_rename = "returns")%>%
  ggplot(aes(x = date, y = returns, fill = symbol))+geom_density(stat = "identity") +
  theme_bw()


stock_prices2 %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "quarterly",
               type       = "arithmetic",
               col_rename = "returns")%>%
  ggplot(aes(x = date, y = returns, fill = symbol))+geom_density(stat = "identity",alpha=0.6) +
  labs(title = " Monthly Returns",
       subtitle = "",
       y = "Monthly Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2,scales = "free") +
  theme_tq() + 
  scale_fill_tq()


library(ggthemes)

stockd<- tq_get(symbols, get = "stock.prices", from = start_date, to = end_date)
stockd%>%ggplot(aes(x = date, y = adjusted)) +
  geom_line() + # Plot stock price
  geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = 100,show.legend=TRUE) +
  ggthemes::theme_hc() +
  scale_colour_hc()+facet_wrap(~  symbol, ncol = 2,scales = "free") 


# Fetch stock data
stock_prices <- tq_get(symbols, get = "stock.prices", from = start_date, to = end_date)

# Extract adjusted closing prices
stock_prices_adj_close <- stock_prices %>%
  select(symbol, date, adjusted) %>%
  spread(symbol, adjusted)

# Calculate daily returns
stock_returns <- na.omit(Return.calculate(stock_prices_adj_close[-1], method = "log"))


# Extract the returns for the portfolio symbols
portfolio_returns <- stock_returns[, symbols]

# Calculate mean return vector
mean_returns <- colMeans(portfolio_returns)

# Calculate sample covariance matrix
cov_matrix <- cov(portfolio_returns)

# Print results
print(portfolio_returns)
print(mean_return)
print(cov_matrix)
