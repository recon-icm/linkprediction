#' Katz Index
#'
#' Similarity measure based on all paths in a graph
#'
#' This function counts all the paths between given pair of nodes, with shorter
#'   paths counting more heavily. Weigths are exponential.
#'
#' @template sim
#' @param beta damping factor
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_katz(g, 0.001)
#'
#' @export

similarity_katz <- function(graph, beta = 0.001){
  A  <- igraph::get.adjacency(graph)
  #I <- Diagonal(nrow(A))
  I <- diag(nrow(A))
  tmp <- I - beta * A
  score <- solve(tmp)
  diag(score) <- 0
  score
}
