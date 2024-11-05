# Geodesic distance vertex similarity
#
# This function calculates similarity score for vertices based on the
#   shortest paths between them.
#
# Geodesic distance between vertices is the length of the shortest paths
#   between them. If there is no path between vertices (ie. thay are in
#   different connected components), a geodesic distance is set to the number
#   of vertices in the graph plus one. Distances are inversed afterwards, so as
#   two vertices are more similar when the distances between them is shorter.
#

similarity_dist <- function(graph, v1, v2, ...){
  score <- igraph::distances(graph, v = v1, to = v2)
  score[is.infinite(score)] <- igraph::vcount(graph) + 1
  # maybe infinity here ?
  score <- 1 / score
  diag(score) <- 0
  score
}
