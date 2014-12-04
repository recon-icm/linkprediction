#' Jaccard index
#'
#' Similarity measure counting the proportion of shared nodes.
#'
#' This is a simple wrapper to an \pkg{igraph} function
#'   \code{\link[igraph]{similarity.jaccard}}, which calculates the proportion
#'   of shared neighbors of two vertices.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_jaccard(g)
#'

similarity_jaccard <- function(graph){
  score <- igraph::similarity.jaccard(graph)
  score
}
