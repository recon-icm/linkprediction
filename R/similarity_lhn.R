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
#' similarity_leicht_holme_newman(g)
#'
#' @export

similarity_leicht_holme_newman <- function(graph){
  deg <- igraph::degree(graph)

  score <- cocitation(graph)
  score <- score / outer(deg, deg)
  score
}
