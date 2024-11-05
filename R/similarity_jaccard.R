# Jaccard index
#
# Similarity measure counting the proportion of shared nodes.
#
# This is a simple wrapper to an \pkg{igraph} function
#   \code{\link[igraph]{similarity}} with \code{method="jaccard"}, 
#   which calculates the proportion
#   of shared neighbors of two vertices.
#

similarity_jaccard <- function(graph, v1, v2, ...){
  score <- igraph::similarity(graph, method = "jaccard")
  score[v1, v2]
}
