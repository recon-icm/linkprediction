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
#' similarity_adamic_adar(g)
#'
#' @export

similarity_adamic_adar <- function(graph){
  score <- igraph::similarity.invlogweighted(graph)
  score
}
