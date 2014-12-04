#' Node proximity
#'
#' General function for calculating nodes' proximity in a graph
#'
#' This function calculates vertex proximity (similarity) with selected method
#'   and between selected vertices.
#'
#' @param graph an object of class \code{igraph} or \code{network}
#' @param method a method (single string) for calculating similarities, see Details
#' @param v1,v2 vectors of vertices between which similarity will be calculated
#'   character is treated as names, numeric as ids
#' @param value a character string giving a type of the object that should be
#'  returned. This must be on of "\code{matrix}", "\code{graph}" or
#'  "\code{edgelist}", with default "\code{matrix}".
#' @param ... additional arguments specific for a selected measure
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
#' @return matrix or edgelist or, if sets of vertices are full, an igraph
#'
#' @export

proxfun <- function(graph, ...){
  UseMethod("proxfun")
}


#' @method proxfun igraph
#' @export
#' @rdname proxfun
proxfun.igraph <- function(graph, method, v1 = NULL, v2 = v1, value = NULL, ...){

  # find method
  method <- match_method(method)

  # check vertices lists
  v1 <- check_vertices(graph, v1)
  v2 <- check_vertices(graph, v2)

  result <- do.call(method, list(graph = graph, v1 = v1, v2 = v2, ...))
  result
}


#' @method proxfun network
#' @export
#' @rdname proxfun
proxfun.network <- function(graph, method, v1 = NULL, v2 = v1, value = NULL, ...){
  graph <- intergraph::asIgraph(graph)
  proxfun.igraph(graph, method = method , v1 = v1, v2 = v2, value = value, ...)
}




## function for matching methods
match_method <- function(method){
  method <- match.arg(method, c("act", "act_n", "aa", "cn", "cos", "cos_l",
                                "dist", "hdi", "hpi", "jaccard", "katz", "l",
                                "lhn_local", "lhn_global", "lp", "mf", "pa", "ra",
                                "rwr", "sor"))
  paste0("similarity_", method)
}



## function checking correctness of vertices list
check_vertices <- function(graph, v){
  if (is.null(v)){
    return(seq_along(igraph::V(graph)))
  } else if (is.numeric(v)){
    v <- as.integer(v)
    if (min(v) < 0 | max(v) > igraph::vcount(graph))
      stop("Vertex id out of range")
    v
  } else if (is.character(v)){
    if (!igraph::is.named(graph))
      stop("Graph does not have vertex names. Provide vertex IDs")
    if (!all(v %in% igraph::V(graph)$name))
      stop("Invalid vertex names")
    v
  } else stop("Vertex sequence must be numeric or character")
}
