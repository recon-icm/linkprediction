#' Hub Depressed Index
#'
#' Similarity measure based on common neighbors
#'
#' This measures assigns lower scores to links adjacent to hubs (high degree
#'   nodes). It counts common neighbors of two vertices and weigths the result
#'   by the higher of those vertices' degrees.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_hdi(g)
#'

similarity_hdi <- function(graph){
  deg <- igraph::degree(graph)

  score <- igraph::cocitation(graph)
  score <- score / outer(deg, deg, pmin)
  score
}
