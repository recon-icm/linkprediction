# Sorensen index/ Dices coefficient
#
# Similarity measure counting the proportion of shared nodes.
#
# This function measures a relative size of an intersection of neighbors sets
#   of two vertices. It is a wrapper of an \pkg{igraph} function
#   \code{\link[igraph]{similarity.dice}}.
#

similarity_sor <- function(graph, v1, v2, ...){
  score <- igraph::similarity.dice(graph)
  score[v1, v2]
}
