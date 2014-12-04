#' Adamic-Adar index
#'
#' Similarity measure counting common neighbors weighted by their degrees.
#'
#' Adamic-Adar Index counts common neighbors with weigths equal to logarithms of
#'   neighbors' degrees. This is a wrapper to an \pkg{igraph} function
#'   \code{\link[igraph]{similarity.invlogweighted}}.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_aa(g)
#'

similarity_aa <- function(graph){
  score <- igraph::similarity.invlogweighted(graph)
  score
}
