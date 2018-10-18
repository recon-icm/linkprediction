#' University of Warsaw co-authorship network
#'
#'
#' Giant component of University of Warsaw (UW) co-authorship network based on
#' publications from years 2007-2009 (period 1) and 2010-2012 (period 2).
#'
#' The basis of this network is a co-authorship graph built from all articles,
#' books, and chapters in edited volumes published in years 2007-2012 that have
#' at least one employee of University of Warsaw as a (co)author.
#'
#' @docType data
#' @name uw
#'
#' @format An `igraph` object with undirected graph with 1486 vertices and 7505
#'   edges, and the following attributes:
#'   
#'   - `affiliation` -- Vertex attribute identifying groups of departments:
#'   natural sciences, social sciences, humanities, other (other departments of
#'   UW), and external (co-authors who are not employees of UW)
#'   - `color`, `size`, `label` -- Vertex attributes for easy plotting. Color corresponds
#'   to the `affiliation` attribute.
#'   - `p1` -- Logical edge attribute. It is `TRUE` if researchers incident on
#'   that edge co-authored at least one publication in period 1.
#'   - `p2` -- Logical edge attribute. It is `TRUE` if researchers incident on
#'   that edge co-authored at least one publication in period 2.
#'   
#'
#' @source Polish Scholarly Bibliography \url{https://pbn.nauka.gov.pl}.
#' 
#' @examples
#' # Plot it
#' data(uw)
#' set.seed(666)
#' xy <- igraph::layout_with_fr(uw)
#' plot(uw, layout=xy, vertex.frame.color=par("bg"))
#' legend(
#'   "topright",
#'   title = "Affiliation",
#'   legend = unique(igraph::V(uw)$affiliation),
#'   pt.bg = unique(igraph::V(uw)$color),
#'   pch = 21,
#'   bty = "n"
#' )
NULL
