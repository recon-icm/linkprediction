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
#' @details 
#' Following methods are available:
#' \describe{
#'  \item{\code{aa}}{Adamic-Adar index}
#'  \item{\code{act}}{average commute time}
#'  \item{\code{act_n}}{normalized average commute time}
#'  \item{\code{cn}}{common neighbours}
#'  \item{\code{cos}}{cosine similarity}
#'  \item{\code{cos_l}}{cosine similarity on L+}
#'  \item{\code{dist}}{graph distance}
#'  \item{\code{hdi}}{Hub Depressed Index}
#'  \item{\code{hpi}}{Hub Promoted Index}
#'  \item{\code{jaccard}}{Jaccard coefficient}
#'  \item{\code{katz}}{Katz index}
#'  \item{\code{l}}{L+ directly}
#'  \item{\code{lhn_local}}{Leicht-Holme-Newman Index}
#'  \item{\code{lhn_global}}{Leicht-Holme-Newman Index global version}
#'  \item{\code{lp}}{Local Path Index}
#'  \item{\code{mf}}{Matrix Forest Index}
#'  \item{\code{pa}}{preferential attachment}
#'  \item{\code{ra}}{resource allocation}
#'  \item{\code{rwr}}{random walk with restart}
#'  \item{\code{sor}}{sorensen index/ dice coefficient}
#' }
#'
#'
#'
#' @return If \code{value = "matrix"} a matrix with \code{length(v1)} rows and
#'   \code{length(v2)} with \code{rownames} and \code{colnames} equal to
#'   \code{v1} and \code{v2} respectively.
#'   If \code{value = "edgelist"} a \code{data.frame} with three columns:
#'   \describe{
#'     \item{from}{ID of a start node of an edge}
#'     \item{to}{ID of an end node of an edge}
#'     \item{value}{similarity score for that edge}
#'     }
#'   Edges with similarity score 0 are omitted.
#'   If \code{value = "graph"} an object of class \code{igraph} or \code{network},
#'   depending on the class of input graph. Returned graph has the same structure
#'   (graph and node attributes, etc.) as the input graph, except for edges -
#'   original edges are skipped, and new edges with positive similarity score
#'   are added. Edged attribute "weight" indicates similarity score.
#'
#' @export

proxfun <- function(graph, ...){
  UseMethod("proxfun")
}


#' @method proxfun igraph
#' @export
#' @rdname proxfun
proxfun.igraph <- function(graph, method, v1 = NULL, v2 = v1,
                           value =c("matrix", "edgelist", "graph"), ...){
  # temporary issue - break code if graph is directed or disconnected until
  # proper functions are implemented
  if (igraph::is.directed(graph) | !igraph::is.connected(graph))
    stop("Graph has to be undirected and connected")

  value <- match.arg(value)


  # find method
  method <- match_method(method)

  # check vertices lists
  v1 <- check_vertices(graph, v1)
  v2 <- check_vertices(graph, v2)

  result <- do.call(method, list(graph = graph, v1 = v1, v2 = v2, ...))
  rownames(result) <- v1
  colnames(result) <- v2
  result <- coerce_result(result, value, graph)
  result
}


#' @method proxfun network
#' @export
#' @rdname proxfun
proxfun.network <- function(graph, method, v1 = NULL, v2 = v1,
                            value =c("matrix", "edgelist", "graph"), ...){
  graph <- intergraph::asIgraph(graph)
  proxfun.igraph(graph, method = method , v1 = v1, v2 = v2, value = value, ...)
}
