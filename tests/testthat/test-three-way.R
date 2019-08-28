context("test-three-way")

d1 <- shuffle_sync(three_way, A, B, C)
d2 <- cbind(three_way, A2 = d1[, "A"])
d2$X <- paste(d2$A, d2$A2, sep = "-")
cells <- split(d2$X, list(d2$B, d2$C))

test_that("shuffle sync works", {
  expect_setequal(cells[[2]], cells[[1]])
  expect_setequal(cells[[3]], cells[[1]])
  expect_setequal(cells[[4]], cells[[1]])
})
