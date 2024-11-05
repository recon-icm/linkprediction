# Cosine similarity  on \eqn{L^+}
#
# Similarity measure based on the pseudoinverse of the Laplacian matrix
#
# This function measures the cosine of the angle between columns of
#   the pseudoinverse of the Laplacian matrix.
#

similarity_cos_l <- function(graph, v1, v2, ...){
  L <- igraph::laplacian_matrix(graph)
  n <- igraph::vcount(graph)
  m <- igraph::ecount(graph)

  L_psinv <- solve(L - 1/n) + 1/n
  dL <- diag(L_psinv)
  score <- L_psinv / outer(dL, dL, function(x, y) sqrt(x * y))
  score[v1, v2]
}
