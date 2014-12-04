#' Local Paths Index
#'
#' Similarity measure based on the neighborhood of order 2
#'
#' This function counts the number of two-paths and three-paths between nodes,
#'   with three-paths weighted by a parameter \eqn{\epsilon}.
#'
#' @template sim
#' @param eps the importance of paths of length three
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_lp(g, eps = 0.01)
#'

similarity_lp <- function(graph, eps = 0.01){
  A <- igraph::get.adjacency(graph)
  score <- A %*% A
  score <- score + score %*% A * eps
  score
}
