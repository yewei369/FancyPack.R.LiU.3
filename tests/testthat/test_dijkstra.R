context("dijkstra")

wiki_graph <-
    data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
               v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
               w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

test_that("Generate correct output", {
    expect_equal(dijkstra(wiki_graph,1), c(0,7,9,20,20,11))
    expect_equal(dijkstra(wiki_graph,3), c(9,10,0,11,11,2))
})


test_that("Generate errors in abnormal scenarioes", {
    expect_error(dijkstra(wiki_graph, 8)) 
    expect_error(dijkstra(wiki_graph, "3"))
    expect_error(dijkstra(as.list(wiki_graph), 3))
})
