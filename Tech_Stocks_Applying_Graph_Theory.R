# Load  required packages
library(quantmod)
library(igraph)

# Define the stock symbols and date range
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Fetch the historical price data
getSymbols(symbols, from = start_date, to = end_date)

# Extract closing prices
prices <- Cl(merge(AMD, NVDA, MSFT, GOOGL, QCOM, BABA, TSM, META, NFLX, ADBE, PYPL, AVGO, MU, AMAT, INTC, STX, LUMN, ETSY, LRCX, CSCO))

# Calculate stock returns
returns <- na.omit(ROC(prices, type = "discrete"))

# Calculate correlation matrix
cor_matrix <- cor(returns)

# Convert correlation matrix to a graph
cor_graph <- graph.adjacency(cor_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)

# Plot the graph
plot(cor_graph, layout = layout.fruchterman.reingold, edge.width = E(cor_graph)$weight*10, edge.color = "gray", vertex.color = "lightblue", vertex.size = 10, vertex.label.cex = 0.8)

# Create a correlation network graph
cor_network <- graph.adjacency(cor_matrix, mode = "undirected", weighted = TRUE)

# Calculate Returns
returns <- do.call(merge, lapply(symbols, function(sym) dailyReturn(Ad(get(sym)))))

# Create a correlation matrix of the returns
correlation_matrix <- cor(returns, use = "pairwise.complete.obs")

# Create an adjacency matrix based on the correlation matrix
adjacency_matrix <- as.matrix(correlation_matrix)

# Create a graph object from the adjacency matrix using the graph.adjacency 
graph <- graph.adjacency(adjacency_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# Plot igraph
plot(graph, vertex.label = symbols, vertex.size = 10, vertex.color = "lightblue", edge.width = E(graph)$weight * 3)


sharpe_ratios <- apply(returns, 2, function(x) {
  sharpe_ratio <- sqrt(252) * mean(x) / sd(x)
  return(sharpe_ratio)
})

# Create a graph object
g <- graph.empty()

# Add vertices (nodes) to the graph
nodes <- symbols
g <- add.vertices(g, length(nodes), name = nodes)

# Add edges (connections) to the graph based on Sharpe Ratios
edges <- combn(symbols, 2)
for (i in 1:ncol(edges)) {
  edge <- edges[, i]
  src <- edge[1]
  tgt <- edge[2]
  if (sharpe_ratios[src] > sharpe_ratios[tgt]) {
    g <- add.edges(g, c(src, tgt))
  } else {
    g <- add.edges(g, c(tgt, src))
  }
}

# Plot the graph
plot(g, vertex.label = symbols, layout = layout.circle)

# Plotting the Sharpe Ratio as a bar chart
barplot(sharpe_ratio, names.arg = symbols, xlab = "Symbols", ylab = "Sharpe Ratio")

# Creating a correlation matrix of returns
cor_matrix <- cor(returns)

# Creating a graph based on the correlation matrix
graph <- graph.adjacency(cor_matrix, weighted = TRUE, mode = "undirected")

# Plotting the graph
plot(graph, vertex.label = symbols, vertex.size = 10, vertex.color = "lightblue", edge.color = "gray", edge.width = 1)


barplot(sharpe_ratios, names.arg = symbols, xlab = "Symbols", ylab = "Sharpe Ratio")

cor_matrix <- cor(returns)
threshold <- 0.7
adj_matrix <- ifelse(abs(cor_matrix) > threshold, 1, 0)
diag(adj_matrix) <- 0

graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
plot(graph, vertex.label = symbols, layout = layout.fruchterman.reingold)
