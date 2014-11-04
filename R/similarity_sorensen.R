#' S\{o}rensen index/ Dice's coefficient
#'
#' Similarity measure counting the proportion of shared nodes.
#'
#' This function measures a relative size of an intersection of neighbors' sets
#'   of two vertices. It is a wrapper of an \code{igraph} function
#'   \code{\link[igraph]{similarity.dice}}.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_sorensen(g)
#'
#' @export

similarity_sorensen <- function(graph){
  score <- igraph::similarity.dice(graph)
  score
}
