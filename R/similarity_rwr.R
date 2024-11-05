# Random Walk with Restart
#
# Similarity measure based on random walker returning to its root
#
# It is an adaptation of PageRank algorithm. This function calculates
#   similarity between two nodes \eqn{a} and \eqn{b} as a sum of stationary
#   distributions of one node in random walk process starting from the other
#   node. Random walker could return to its root at every step with fixed
#   fixed probability \eqn{\alpha}.
#

similarity_rwr <- function(graph, v1, v2, alpha = 0.3, ...){
  P <- as.matrix(igraph::stochastic_matrix(graph))
  score <- solve(diag(nrow(P)) - (1 - alpha) * t(P))
  score <- score * alpha
  score <- score + t(score)
  score[v1, v2]
}
