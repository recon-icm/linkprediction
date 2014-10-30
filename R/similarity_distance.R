#' Geodesic distance vertex similarity
#'
#' This function calculates similarity score for vertices based on the
#'   shortest paths between them.
#'
#' Geodesic distance between vertices is the length of the shortest paths
#'   between them. If there is no path between vertices (ie. thay are in
#'   different connected components), a geodesic distance is set to the number
#'   of vertices in the graph plus one. Distances are inversed afterwards, so as
#'   two vertices are more similar when the distances between them is shorter.
#'
#' @param graph Igraph object.
#'
#' @return A square numeric matrix with size equal to the number of vertices
#'   in the input graph.
#'
#' @seealso \code{\link[igraph]{similarity}}
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_distance(g)
#'
#' @export
#' @importFrom igraph shortest.paths
#' @importFrom igraph vcount


similarity_distance <- function(graph){
  score <- shortest.paths(graph)
  score[is.infinite(score)] <- vcount(graph) + 1
  # maybe infinity here ?
  score <- 1 / score
  diag(score) <- 0
  score
}
