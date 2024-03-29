# Hub Promoted Index
#
# Similarity measure based on common neighbors
#
# This measures assigns higher scores to links adjacent to hubs (high degree
#   nodes). It counts common neighbors of two vertices and weigths the result
#   by the lower of those vertices degrees.
#

similarity_hpi <- function(graph, v1, v2, ...){
  deg <- igraph::degree(graph)

  score <- igraph::cocitation(graph, v = v1)
  score <- score[, v2]
  score <- score / outer(deg[v1], deg[v2], pmin)
  score
}
