#' Cosine vertex similarity/ Salton index
#'
#' Similarity measure based on resource allocation process.
#'
#' This function measures the cosine of the angle between columns of
#'   the adjacency matrix, corresponding to given nodes.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_cos(g)
#'

similarity_cos <- function(graph){
  deg <- igraph::degree(graph)

  score <- igraph::cocitation(graph)
  score <- score / outer(deg, deg, function(x, y) sqrt(x * y))
  score
}
