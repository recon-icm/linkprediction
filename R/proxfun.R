#' Node proximity
#'
#' General function for calculating nodes' proximity in a graph
#'
#' This function calculates vertex proximity (similarity) with selected method
#'   and between selected vertices.
#'
#' @param graph an object of class igraph
#' @param method a method (single string) for calculating similarities, see Details
#' @param v1,v2 vectors of vertices between which similarity will be calculated
#'   character is treated as names, numeric as ids
#' @param ... additional arguments specific for a selected method
#'
#' @details Available methods are:
#' \itemize{
#'   \item \code{act} average commute time
#'   \item \code{act_n} normalized average commute time
#'   \item \code{aa} Adamic-Adar index
#'   \item \code{cn} common neighbours
#'   \item \code{cos} cosine similarity
#'   \item ...
#'   }
#'
#' @return matrix
#'
#' @export

proxfun <- function(graph, method, v1 = NULL, v2 = v1, ...){
  if (!inherits(graph, "igraph")) stop("A graph must be of class igraph")

  method <- match_method(method)
  result <- do.call(method, list(graph = graph, ...))



}



match_method <- function(method){
  method <- match.arg(method, c("act", "act_n", "aa", "cn"))
  paste0("similarity_", method)
}
