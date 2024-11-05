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
#' This function calculates vertex proximities in graph \code{graph} with the 
#' selected \code{method}. The \code{graph} has to be undirected and connected. 
#' Some of the methods support computation only for selected vertices, which 
#' should be more efficient when needed. Supplying vertex IDs or names (if 
#' present in the \code{graph}) to \code{v1} and \code{v2} will calculate
#' proximities of \eqn{v1 x v2}.
#' 
#' The following \code{method}s are available (see \code{vignette("proxfun",
#' package="linkprediction")} for more details and formal definitions):
#'
#' \describe{
#'  \item{\code{aa}}{Adamic-Adar index  (Adamic and Adar 2001). Additional arguments are passed to [igraph::similarity].}
#'  \item{\code{act}}{Average Commute Time (Fouss, Pirotte, Renders, and Saerens 2007)}
#'  \item{\code{act_n}}{Normalized Average Commute Time (Fouss et al. 2007)}
#'  \item{\code{cn}}{Common Neighbours}
#'  \item{\code{cos}}{Cosine similarity (Salton and McGill 1986)}
#'  \item{\code{cos_l}}{cosine similarity on L+ (Fouss et al. 2007)}
#'  \item{\code{dist}}{graph distance}
#'  \item{\code{hdi}}{Hub Depressed Index (Ravasz, Somera, Mongru, Oltvai, and Barabasi 2002)}
#'  \item{\code{hpi}}{Hub Promoted Index (Ravasz et al. 2002)}
#'  \item{\code{jaccard}}{Jaccard coefficient (Jaccard 1912)}
#'  \item{\code{katz}}{Katz index (Katz 1953)}
#'  \item{\code{l}}{L+ directly (Fouss et al. 2007)}
#'  \item{\code{lhn_local}}{Leicht-Holme-Newman Index (Leicht, Holme, and Newman 2006)}
#'  \item{\code{lhn_global}}{Leicht-Holme-Newman Index global version (Leicht et al. 2006)}
#'  \item{\code{lp}}{Local Path Index (Zhou, Lu, and Zhang 2009)}
#'  \item{\code{mf}}{Matrix Forest Index (Chebotarev P. Yu. 1997)}
#'  \item{\code{pa}}{preferential attachment (Barabasi and Albert 1999)}
#'  \item{\code{ra}}{resource allocation (Zhou et al. 2009)}
#'  \item{\code{rwr}}{random walk with restart (Brin and Page 1998). Additional argument `alpha` (default value 0.3) is the probability that the walk will restart after a step.}
#'  \item{\code{sor}}{sorensen index/dice coefficient (Sorensen 1948)}
#' }
#'
#'
#'
#' @return 
#' If \code{value = "matrix"} a matrix with \code{length(v1)} rows and
#' \code{length(v2)} with \code{rownames} and \code{colnames} equal to integer
#' node IDs. If \code{value = "edgelist"} a \code{data.frame} with three
#' columns:
#' \describe{
#'  \item{from}{ID of a start node of an edge}
#'  \item{to}{ID of an end node of an edge}
#'  \item{value}{similarity score for that edge}
#' }
#' Edges with similarity score 0 are omitted. If \code{value = "graph"} an
#' object of class \code{igraph} or \code{network}, depending on the class of
#' input graph. Returned graph has the same structure (graph and node 
#' attributes, etc.) as the input graph, except for edges - original edges are
#' skipped, and new edges with positive similarity score are added. Edged
#' attribute "weight" indicates similarity score.
#'   
#' @references 
#' Adamic L and Adar E (2003). "Friends and Neighbors on the Web." _Social Networks_, *25*,
#' pp. 211-230 \doi{10.1016/S0378-8733(03)00009-1}.
#' 
#' Barabasi A and Albert R (1999). "Emergence of Scaling in Random Networks." _Science_,
#' *286*(5439), pp. 509-512.
#' 
#' Brin S and Page L (1998). "The anatomy of a large-scale hypertextual Web search engine ."
#' _Computer Networks and ISDN Systems _, *30*(1-7), pp. 107 - 117. Proceedings of the
#' Seventh International World Wide Web Conference .
#' 
#' Chebotarev P. Yu. SEV (1997). "The matrix-forest theorem and measuring relations in small
#' social groups ." _Automation and Remote Control _, *58*(9), pp. 1505-1514.
#' 
#' Fouss F, Pirotte A, Renders J and Saerens M (2007). "Random-Walk Computation of
#' Similarities Between Nodes of a Graph with Application to Collaborative Recommendation."
#' _IEEE Transactions on Knowledge and Data Engineering_, *19*(3), pp. 355-369 \doi{10.1109/TKDE.2007.46}.
#' 
#' Jaccard P (1912). "The Distribution of the Flora in the Alpine Zone 1" _New
#' Phytologist_, *11*(2), pp. 37-50.
#' 
#' Katz L (1953). "A new status index derived from sociometric analysis." _Psychometrika_,
#' *18*(1), pp. 39-43.
#' 
#' Leicht EA, Holme P and Newman MEJ (2006). "Vertex similarity in networks." _Phys. Rev.
#' E_, *73*(2), pp. 026120 \doi{10.1103/PhysRevE.73.026120}.
#' 
#' Ravasz E, Somera AL, Mongru DA, Oltvai ZN and Barabasi A (2002). "Hierarchical
#' Organization of Modularity in Metabolic Networks." _Science_, *297*(5586), pp. 1551-1555.
#' 
#' Salton G and McGill MJ (1986). _Introduction to Modern Information Retrieval_.
#' McGraw-Hill, Inc., New York, NY, USA.
#' 
#' Sorensen T (1948). "A Method of Establishing Groups of Equal Amplitude in Plant Sociology
#' Based on Similarity of Species Content and Its Application to Analyses of the Vegetation
#' on Danish Commons." _Biologiske Skrifter_, *5*, pp. 1-34.
#' 
#' Zhou T, Lu L and Zhang Y (2009). "Predicting missing links via local information." _The
#' European Physical Journal B_, *71*(4), pp. 623-630 \doi{10.1140/epjb/e2009-00335-8}.
#'
#' @export
#' @encoding UTF-8
#' 
#' @examples 
#' if(requireNamespace("igraph")) {
#'   g <- igraph::make_graph(~ A -- C:D:E -- B -- F -- G:H -- I)
#'   
#' # Adamic-Adar
#' proxfun(g, method="aa", value="edgelist")
#'   
#' # Random Walk with Restart
#' proxfun(g, method="rwr", value="edgelist")
#' }

proxfun <- function(graph, ...){
  UseMethod("proxfun")
}


#' @rdname proxfun
#' @export
proxfun.igraph <- function(graph, method, v1 = NULL, v2 = v1,
                           value =c("matrix", "edgelist", "graph"), ...){
  # temporary issue - break code if graph is directed or disconnected until
  # proper functions are implemented
  if (igraph::is_directed(graph) | !igraph::is_connected(graph))
    stop("graph has to be undirected and connected")

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


#' @rdname proxfun
#' @export
proxfun.network <- function(graph, method, v1 = NULL, v2 = v1,
                            value =c("matrix", "edgelist", "graph"), ...){
  if(!requireNamespace("intergraph")) {
    stop("Install package 'intergraph' to work with other types of objects",
         call. = FALSE)
  }
  graph <- intergraph::asIgraph(graph)
  proxfun.igraph(graph, method = method , v1 = v1, v2 = v2, value = value, ...)
}
