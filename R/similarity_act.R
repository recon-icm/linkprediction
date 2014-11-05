#' Averag Commute Time
#'
#' Similarity measure based on random walker
#'
#' This function calculates vertex similarity based on the average number of
#'  steps, that random walker on the graph needs to get from one vertex to
#'  another.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_act(g)
#'
#' @export

similarity_act <- function(graph){
  L <- igraph::graph.laplacian(graph)
  n <- igraph::vcount(graph)
  m <- igraph::ecount(graph)

  L_psinv <- solve(L - 1/n) + 1/n
  score <- 2 * m * (diag(L_psinv) %*% t(rep(1, n)) +
                      rep(1, n) %*% t(diag(L_psinv)) -
                      2 * L_psinv)
  score <- 1 / score
  score
}
