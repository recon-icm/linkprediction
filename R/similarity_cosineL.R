#' Cosine similarity  on \eqn{L^+}
#'
#' Similarity measure based on the pseudoinverse of the Laplacian matrix
#'
#' This function measures the cosine of the angle between columns of
#'   the pseudoinverse of the Laplacian matrix.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_cosineL(g)
#'
#' @export

similarity_cosineL <- function(graph){
  L <- igraph::graph.laplacian(graph)
  n <- igraph::vcount(graph)
  m <- igraph::ecount(graph)

  L_psinv <- solve(L - 1/n) + 1/n
  dL <- diag(L_psinv)
  score <- L_psinv / outer(dL, dL, function(x, y) sqrt(x * y))
  score
}
