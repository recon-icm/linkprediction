# Katz Index
#
# Similarity measure based on all paths in a graph
#
# This function counts all the paths between given pair of nodes, with shorter
#   paths counting more heavily. Weigths are exponential.
#

similarity_katz <- function(graph, v1, v2, beta = 0.001, ...){
  A  <- igraph::as_adjacency_matrix(graph)
  #I <- Diagonal(nrow(A))
  I <- diag(nrow(A))
  tmp <- I - beta * A
  score <- solve(tmp)
  diag(score) <- 0
  score[v1, v2]
}
