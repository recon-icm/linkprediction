# Matrix Forest Index
#
# Similarity measure based on the concept of spanning trees
#
# This measure can be understood as the ratio of the number of spanning rooted
#   forest such that nodes \eqn{x} and \eqn{y} belong to the same tree rooted
#   at \eqn{x} to all spanning rooted forests of the network.
#

similarity_mf <- function(graph, v1, v2, ...){
  L <- igraph::laplacian_matrix(graph)
  n <- igraph::vcount(graph)

  score <- solve(diag(n) + L)
  score[v1, v2]
}
