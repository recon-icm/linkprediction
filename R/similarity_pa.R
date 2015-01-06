# Preferential attachment vertex similarity
#
# This function calculates similarity score for vertices based on their degrees.
#
# Preferential attachment model was designed by Barabsi and Albert to simulate
#   the evolution of scale-free graphs. The probability of new edge occuring
#   between two vertices is proportional to the product of degrees of those
#   vertices. It is a similarity measure under assumption that the most similar
#   vertices are simulataneously the most probable to be connected.
#

similarity_pa <- function(graph, v1, v2, ...){
  deg <- igraph::degree(graph)
  score <- outer(deg[v1], deg[v2])
  diag(score) <- 0
  score
}
