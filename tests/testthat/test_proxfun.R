context("Checking if 'proxfun' handles argument correctly")

g <- igraph::graph(c(1,2, 1,3, 1,4, 2,4, 1,5, 2,5, 4,5), directed = FALSE)
igraph::V(g)$name <- letters[5:1]

test_that("Wrong vertices sequences throw an error",{
  expect_error(proxfun(g, "cn", v1 = 6))
  expect_error(proxfun(g, "cn", v1 = "f"))
  expect_error(proxfun(g, "cn", v1 = 3:6))
  expect_error(proxfun(g, "cn", v1 = c("a", "c", "f")))
  expect_error(proxfun(g, "cn", v1 = 1, v2 = 6))
  expect_error(proxfun(g, "cn", v1 = 1, v2 = "f"))
})


test_that("Error is thrown for empty network", {
  expect_error( proxfun( igraph::make_empty_graph(n=4, directed=FALSE) ) )
  expect_error( proxfun( igraph::make_empty_graph(n=4, directed=TRUE) ) )
})


test_that("Error is thrown for disconnected networks", {
  expect_error( proxfun(igraph::make_graph(c(1,2, 3,4), directed=FALSE)) )
})

test_that("Error is thrown for directed networks", {
  expect_error(
    proxfun( igraph::make_graph(c(1,2, 2,3, 3,4), directed=TRUE) )
  )
})
