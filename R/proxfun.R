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
#'  \code{aa} Adamic-Adar index
#'
#'  \code{act} average commute time
#'
#'  \code{act_n} normalized average commute time
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
    v <- match(v, igraph::V(graph)$name)
    if (any(is.na(v)))
      stop("Invalid vertex names")
    v
  } else stop("Vertex sequence must be numeric or character")
}


## function coercing result to desired type
coerce_result <- function(result, value, graph){
  if (value == "matrix")
    return(result)
  if (value == "edgelist"){
    ind <- c(result > 0)
    edges <- expand.grid(rownames(result), colnames(result))[ind, ]
    names(edges) <- c("from", "to")
    edges$value <- c(result)[ind]
    rownames(edges) <- NULL
    return(edges)
  }
  if (value == "graph"){
    if (any(dim(result) < igraph::vcount(graph))){
      warning("Only similarity for full vertices sequence could be returned as a graph. Returning matrix instead.")
      return(result)
    }
    # sort vertices lists
    result <- result[order(as.numeric(rownames(result))),
                     order(as.numeric(colnames(result)))]
    # create weighted graph
    g <- graph.adjacency(result, weighted = TRUE, add.colnames = NA,
                         mode = ifelse(igraph::is.directed(graph), "directed", "undirected"))
    # empty original graph and add weighted graph to preserve structure
    g <- graph.union(igraph::delete.edges(graph, igraph::E(graph)), g,
                     byname = FALSE)
    return(g)
  }
}

