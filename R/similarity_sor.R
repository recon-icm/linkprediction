#' Sorensen index/ Dice's coefficient
#'
#' Similarity measure counting the proportion of shared nodes.
#'
#' This function measures a relative size of an intersection of neighbors' sets
#'   of two vertices. It is a wrapper of an \pkg{igraph} function
#'   \code{\link[igraph]{similarity.dice}}.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_sor(g)
#'

similarity_sor <- function(graph){
  score <- igraph::similarity.dice(graph)
  score
}
