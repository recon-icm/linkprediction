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
#' @details Following methods are available:
#'
#'  \code{act} average commute time
#'
#'  \code{act_n} normalized average commute time
#'
#'  \code{aa} Adamic-Adar index
#'
#'  \code{cn} common neighbours
#'
#'  \code{cos} cosine similarity
#'
#'  \code{cos_l} cosine similarity on L+
#'
#'  \code{dist} graph distance
#'
#'  \code{hdi} Hub Depressed Index
#'
#'  \code{hpi} Hub Promoted Index
#'
#'  \code{jaccard} Jaccard coefficient
#'
#'  \code{katz} Katz index
#'
#'  \code{l} L+ directly
#'
#'  \code{lhn_local} Leicht-Holme-Newman Index
#'
#'  \code{lhn_global} Leicht-Holme-Newman Index global version
#'
#'  \code{lp} Local Path Index
#'
#'  \code{mf} Matrix Forest Index
#'
#'  \code{pa} preferential attachment
#'
#'  \code{ra} resource allocation
#'
#'  \code{rwr} random walk with restart
#'
#'  \code{sor} sorensen index/ dice coefficient
#'
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
  method <- match.arg(method, c("act", "act_n", "aa", "cn", "cos", "cos_l",
                                "dist", "hdi", "hpi", "jaccard", "katz", "l",
                                "lhn_local", "lhn_global", "lp", "mf", "pa", "ra",
                                "rwr", "sor"))
  paste0("similarity_", method)
}
