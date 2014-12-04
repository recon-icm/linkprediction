#' Random Walk with Restart
#'
#' Similarity measure based on random walker returning to its root
#'
#' It is an adaptation of PageRank algorithm. This function calculates
#'   similarity between two nodes \eqn{a} and \eqn{b} as a sum of stationary
#'   distributions of one node in random walk process starting from the other
#'   node. Random walker could return to its root at every step with fixed
#'   fixed probability \eqn{\alpha}.
#'
#' @template sim
#' @param alpha probability of return of random walker
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_rwr(g, alpha = 0.3)
#'

similarity_rwr <- function(graph, alpha = 0.3){
  P <- as.matrix(igraph::get.stochastic(graph))
  score <- solve(diag(nrow(P)) - (1 - alpha) * t(P))
  score <- score * alpha
  score <- score + t(score)
  score
}
