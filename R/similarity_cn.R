# Common neighbors vertex similarity
#
# Similarity measure counting number of common neighbors.
#
# This is a simple wrapper to an \pkg{igraph} function \code{cocitation},
#   which counts the number of neighbours shared by two vertices.
#
# @seealso \code{\link[igraph]{cocitation}}
#

similarity_cn <- function(graph, v1, v2, ...){
  score <- igraph::cocitation(graph, v = v1)
  score[, v2]
}
