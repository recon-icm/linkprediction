#' Leicht-Holme-Newman Global Index
#'
#' Recursive similarity measure based on all paths in a graph
#'
#' This function calculates vertex similarity based on the concept, that two
#'   nodes are similar, when their neighbors are similar. Smaller \eqn{\theta}
#'   assings more weight on shorter paths (closer neighbors are more important).
#'
#' @template sim
#' @param theta damping factor
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_lhn_global(g, 0.5)
#'

similarity_lhn_global <- function(graph, theta = 0.5){
  lambda <- igraph::graph.eigen(graph)$value
  deg <- igraph::degree(graph)
  A <- igraph::get.adjacency(graph)
  #I <- Diagonal(nrow(adj))
  I <- diag(nrow(A))
  tmp <- I - (theta / lambda) * A
  score <- solve(tmp)
  tmp <- outer(deg, deg)
  score <- score / tmp
  score
}
