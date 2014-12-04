#' Leicht-Holme-Newman Index
#'
#' Similarity measure based on the number of common neighbors.
#'
#' This function counts the number of common neighbours relative to the expected
#'   value (in random graph with the same degree distribution).
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_lhn_local(g)
#'

similarity_lhn_local <- function(graph){
  deg <- igraph::degree(graph)

  score <- igraph::cocitation(graph)
  score <- score / outer(deg, deg)
  score
}
