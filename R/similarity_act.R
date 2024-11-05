# Average Commute Time
#
# Similarity measure based on random walker
#
# This function calculates vertex similarity based on the average number of
#  steps, that random walker on the graph needs to get from one vertex to
#  another.
#

similarity_act <- function(graph, v1, v2, ...){
  L <- igraph::laplacian_matrix(graph)
  n <- igraph::vcount(graph)
  m <- igraph::ecount(graph)

  L_psinv <- solve(L - 1/n) + 1/n
  score <- 2 * m * (diag(L_psinv) %*% t(rep(1, n)) +
                      rep(1, n) %*% t(diag(L_psinv)) -
                      2 * L_psinv)
  score <- 1 / score
  score[v1, v2]
}
