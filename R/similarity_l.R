#' Pseudoinverse of the Laplacian
#'
#' Similarity measure based solely on the pseudoinverse of the Laplacian matrix
#'
#' The pseudo inverse of the Laplacian matrix provides provides a direct measure
#'   of similarity, as its elements are the inner products of vectors from an
#'   Euclidean space, which preserves Average Commute Time between nodes.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_l(g)
#'

similarity_l <- function(graph){
  L <- igraph::graph.laplacian(graph)
  n <- igraph::vcount(graph)

  score <- solve(L - 1/n) + 1/n
  score
}
