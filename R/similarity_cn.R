#' Common neighbors vertex similarity
#'
#' Similarity measure counting number of common neighbors.
#'
#' This is a simple wrapper to an \pkg{igraph} function \code{cocitation},
#'   which counts the number of neighbours shared by two vertices.
#'
#' @template sim
#' @seealso \code{\link[igraph]{cocitation}}
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_cn(g)
#'
#' @export

similarity_cn <- function(graph){
  score <- igraph::cocitation(graph)
  score
}
