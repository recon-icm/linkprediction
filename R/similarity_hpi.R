#' Hub Promoted Index
#'
#' Similarity measure based on common neighbors
#'
#' This measures assigns higher scores to links adjacent to hubs (high degree
#'   nodes). It counts common neighbors of two vertices and weigths the result
#'   by the lower of those vertices' degrees.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_hpi(g)
#'
#' @export

similarity_hpi <- function(graph){
  deg <- igraph::degree(graph)

  score <- igraph::cocitation(graph)
  score <- score / outer(deg, deg, pmax)
  score
}
