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
    if (!igraph::is_named(graph))
      stop("Graph does not have vertex names. Provide vertex IDs")
    v <- match(v, igraph::V(graph)$name)
    if (any(is.na(v)))
      stop("Invalid vertex names")
    v
  } else stop("Vertex sequence must be numeric or character")
}




## Function coercing result to desired type
## result = proxfun() result
## value = argument as in proxfun()
## graph = original graph
coerce_result <- function(result, value, graph){
  if (value == "matrix")
    return(result)
  if (value == "edgelist"){
    ind <- c(result > 0)
    edges <- expand.grid(
      as.numeric(rownames(result)), 
      as.numeric(colnames(result)), 
      stringsAsFactors = FALSE
      )[ind, ]
    names(edges) <- c("from", "to")
    edges$value <- c(result)[ind]
    rownames(edges) <- NULL
    return(edges)
  }
  if (value == "graph"){
    if (any(dim(result) < igraph::vcount(graph))){
      stop("returning as igraph is not supported for proximities calculated for a subset of edges")
      return(result)
    }
    # sort vertices lists
    result <- result[order(as.numeric(rownames(result))),
                     order(as.numeric(colnames(result)))]
    # create weighted graph
    g <- igraph::graph_from_adjacency_matrix(result, weighted = TRUE, add.colnames = NA,
                         mode = ifelse(igraph::is_directed(graph), "directed", "undirected"))
    # empty original graph and add weighted graph to preserve structure
    g <- igraph::union(igraph::delete_edges(graph, igraph::E(graph)), g,
                     byname = FALSE)
    return(g)
  }
}
