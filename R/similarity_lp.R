# Local Paths Index
#
# Similarity measure based on the neighborhood of order 2
#
# This function counts the number of two-paths and three-paths between nodes,
#   with three-paths weighted by a parameter \eqn{\epsilon}.
#

similarity_lp <- function(graph, v1, v2, eps = 0.01, ...){
  A <- igraph::get.adjacency(graph)
  score <- A %*% A
  score <- score + score %*% A * eps
  as.matrix(score[v1, v2])
}
