# Leicht-Holme-Newman Index
#
# Similarity measure based on the number of common neighbors.
#
# This function counts the number of common neighbours relative to the expected
#   value (in random graph with the same degree distribution).
#

similarity_lhn_local <- function(graph, v1, v2, ...){
  deg <- igraph::degree(graph)

  score <- igraph::cocitation(graph, v = v1)
  score <- score[, v2]
  score <- score / outer(deg[v1], deg[v2])
  score
}
