#' Averag Commute Time, normalized
#'
#' Similarity measure based on random walker
#'
#' This function calculates vertex similarity based on the average number of
#'  steps, that random walker on the graph needs to get from one vertex to
#'  another and additionally weights it by its stationary distribution.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_act_n(g)
#'
#' @export

similarity_act_n <- function(graph){
  L <- igraph::graph.laplacian(graph)
  n <- igraph::vcount(graph)
  m <- igraph::ecount(graph)
  deg <- igraph::degree(graph)

  L_psinv <- solve(L - 1/n) + 1/n

  # tau - first passage times (derivation of formula in papers)
  tmp <- L_psinv %*% as.matrix(deg) %*% matrix(1,1,n)
  tau <- tmp - t(tmp) - 2*m* (L_psinv - matrix(1,n,1)%*%diag(L_psinv))

  # pi - stationary distribution = normalized degree
  pi <- deg / sum(deg)

  # normalizing - multiplicate every row of tau by pi
  tmp <- tau %*% diag(pi)
  tau <- tmp + t(tmp)

  score <- 1 / tau
  diag(score) <- 0
  score
}
