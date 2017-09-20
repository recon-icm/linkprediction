#' Vertex proximity indexes
#'
#' General function for calculating several types of vertex proximities in a graph.
#'
#' @param graph an object of class \code{igraph} or \code{network}
#' @param method single character, the method to be used, see Details
#' @param v1,v2 vectors of vertices between which similarity will be calculated.
#' Character vector is interpreted as vertex names. Numeric vector as vertex ids.
#' @param value a character string giving a type of the object that should be
#'  returned. This must be one of "\code{matrix}", "\code{graph}" or
#'  "\code{edgelist}", with default "\code{matrix}".
#' @param ... additional arguments specific for a selected measure
#'
#' @details 
#' \Sexpr[stage=build,results=verbatim]{{
#' keys <- c("adamic", "fouss", "pagerank", "zhou2009", "sorensen", "salton", "ravasz", "barabasi1999", "jaccard", "katz", "leicht", "mfi")
#' bibdb <- bibtex::read.bib("vignettes/refs.bib")
#' bibdb <- bibdb[keys]
#' } }
#' 
#' This function calculates vertex proximity with the
#' selected method and, optionally, between vertices
#' specified with \code{v1} and \code{v2}. The following \code{method}s
#' are available:
#'
#' \describe{
#'  \item{\code{aa}}{Adamic-Adar index \Sexpr[stage=build]{cite("adamic", bibdb)}}
#'  \item{\code{act}}{Average Commute Time \Sexpr[stage=build]{cite("fouss", bibdb)}}
#'  \item{\code{act_n}}{Normalized Average Commute Time \Sexpr[stage=build]{cite("fouss", bibdb)}}
#'  \item{\code{cn}}{Common Neighbours}
#'  \item{\code{cos}}{Cosine similarity \Sexpr[stage=build]{cite("salton", bibdb)}}
#'  \item{\code{cos_l}}{cosine similarity on L+ \Sexpr[stage=build]{cite("fouss", bibdb)}}
#'  \item{\code{dist}}{graph distance}
#'  \item{\code{hdi}}{Hub Depressed Index \Sexpr[stage=build]{cite("ravasz", bibdb)}}
#'  \item{\code{hpi}}{Hub Promoted Index \Sexpr[stage=build]{cite("ravasz", bibdb)}}
#'  \item{\code{jaccard}}{Jaccard coefficient \Sexpr[stage=build]{cite("jaccard", bibdb)} \Sexpr[stage=build]{cite("adamic", bibdb)}}
#'  \item{\code{katz}}{Katz index \Sexpr[stage=build]{cite("katz", bibdb)}}
#'  \item{\code{l}}{L+ directly \Sexpr[stage=build]{cite("fouss", bibdb)}}
#'  \item{\code{lhn_local}}{Leicht-Holme-Newman Index \Sexpr[stage=build]{cite("leicht", bibdb)}}
#'  \item{\code{lhn_global}}{Leicht-Holme-Newman Index global version \Sexpr[stage=build]{cite("leicht", bibdb)}}
#'  \item{\code{lp}}{Local Path Index \Sexpr[stage=build]{cite("zhou2009", bibdb)}}
#'  \item{\code{mf}}{Matrix Forest Index \Sexpr[stage=build]{cite("mfi", bibdb)}}
#'  \item{\code{pa}}{preferential attachment \Sexpr[stage=build]{cite("barabasi1999", bibdb)}}
#'  \item{\code{ra}}{resource allocation \Sexpr[stage=build]{cite("zhou2009", bibdb)}}
#'  \item{\code{rwr}}{random walk with restart \Sexpr[stage=build]{cite("pagerank", bibdb)}}
#'  \item{\code{sor}}{sorensen index/ dice coefficient \Sexpr[stage=build]{cite("sorensen", bibdb)}}
#' }
#'
#'
#'
#' @return 
#' If \code{value = "matrix"} a matrix with \code{length(v1)} rows and
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
#' @references 
#' 
#' \Sexpr[stage=build,results=rd]{capture.output(print(bibdb, sort=TRUE))}
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
