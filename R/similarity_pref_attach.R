#' Preferential attachment vertex similarity
#'
#' This function calculates similarity score for vertices based on their degrees.
#'
#' Preferential attachment model was designed by Barabsi and Albert to simulate
#'   the evolution of scale-free graphs. The probability of new edge occuring
#'   between two vertices is proportional to the product of degrees of those
#'   vertices. It is a similarity measure under assumption that the most similar
#'   vertices are simulataneously the most probable to be connected.
#'
#' @param graph Igraph object.
#'
#' @return A square numeric matrix with size equal to the number of vertices
#'   in the input graph.
#'
#' @seealso \code{\link[igraph]{similarity}}
#'
#' @examples
#' g <- igraph::barabasi.game(20)
#' similarity_pref_attach(g)
#'
#' @export
#' @importFrom igraph degree

similarity_pref_attach <- function(graph){
  deg <- degree(graph)
  score <- outer(deg, deg)
  diag(score) <- 0
  score
}
