# Adamic-Adar index
#
# Similarity measure counting common neighbors weighted by their degrees.
#
# Adamic-Adar Index counts common neighbors with weigths equal to logarithms of
#   neighbors degrees. This is a wrapper to an \pkg{igraph} function
#   \code{\link[igraph]{similarity.invlogweighted}}.
#


similarity_aa <- function(graph, v1, v2, ...){
  score <- igraph::similarity.invlogweighted(graph)
  score[v1, v2]
}
