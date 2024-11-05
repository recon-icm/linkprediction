# Leicht-Holme-Newman Global Index
#
# Recursive similarity measure based on all paths in a graph
#
# This function calculates vertex similarity based on the concept, that two
#   nodes are similar, when their neighbors are similar. Smaller \eqn{\theta}
#   assings more weight on shorter paths (closer neighbors are more important).
#

similarity_lhn_global <- function(graph, v1, v2, theta = 0.5, ...){
  lambda <- igraph::spectrum(graph)$value
  deg <- igraph::degree(graph)
  A <- igraph::as_adjacency_matrix(graph)
  #I <- Diagonal(nrow(adj))
  I <- diag(nrow(A))
  tmp <- I - (theta / lambda) * A
  score <- solve(tmp)
  tmp <- outer(deg, deg)
  score <- score / tmp
  score[v1, v2]
}
