# Adamic-Adar index
#
# Similarity measure counting common neighbors weighted by their degrees.
#
# Adamic-Adar Index counts common neighbors with weigths equal to logarithms of
#   neighbors degrees. This is a wrapper to an \pkg{igraph} function
#   \code{\link[igraph]{similarity.invlogweighted}}.
#


similarity_aa <- function(graph, v1, v2, ...){
  all_v <- unique(c(v1, v2))
  score <- igraph::similarity(
    graph, 
    vids=all_v, 
    method="invlogweighted",
    ... 
    )
  r <- match(v1, all_v)
  k <- match(v2, all_v)
  score[r, k]
}
