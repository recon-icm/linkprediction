# Cosine vertex similarity/ Salton index
#
# Similarity measure based on resource allocation process.
#
# This function measures the cosine of the angle between columns of
#   the adjacency matrix, corresponding to given nodes.
#

similarity_cos <- function(graph, v1, v2, ...){
  deg <- igraph::degree(graph)

  score <- igraph::cocitation(graph, v = v1)
  score <- score[, v2]
  score <- score / outer(deg[v1], deg[v2], function(x, y) sqrt(x * y))
  score
}
