#' Resource allocation vertex similarity
#'
#' Similarity measure based on resource allocation process.
#'
#' This function counts the number of common neighbours weighted by the inverse
#' of their degrees.
#'
#' @template sim
#'
#' @examples
#' g <- igraph::random.graph.game(20, 0.3)
#' similarity_ra(g)
#'

similarity_ra <- function(graph){
  n <- igraph::vcount(graph)
  score <- matrix(integer(n^2), nrow = n)

  neighbors <- igraph::neighborhood(graph, 1)
  neighbors <- lapply(neighbors, function(x) x[-1])

  degrees <- igraph::degree(graph)
  ## inverse map
  #invmap <- match(seq_along(neighbors), vertices_index)
  for (k in seq(n)){
    tmp <- neighbors[[k]]
    l <- degrees[[k]]
    if (l > 1){
      for (i in 1:(l-1)){
        n1 <- tmp[i]
#         mapi <- invmap[[tmp[i]]]
#         if (!is.na(mapi)){
          for (j in (i+1):l){
#             mapj <- invmap[[tmp[j]]]
#             if (!is.na(mapj)){
#               sim1[mapi, mapj] <- sim1[mapi, mapj] + 1/l
#               sim1[mapj, mapi] <- sim1[mapj, mapi] + 1/l
#             }
            n2 <- tmp[j]
            score[n1, n2] <- score[n1, n2] + 1/l
            score[n2, n1] <- score[n2, n1] + 1/l
          }
#         }
      }
    }
  }
score
}
