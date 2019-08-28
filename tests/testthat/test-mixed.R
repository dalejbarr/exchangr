context("test-mixed")

.deconstruct <- function(d1) {
  d1$rn <- paste0("R", rep(1:3, 8))
  d2 <- reshape(d1[, c("subject", "A", "rn")],
                idvar = "subject", timevar = "rn", direction = "wide")
  d2$X <- paste(d2$A.R1, d2$A.R2, d2$A.R3, sep = "-")
  split(d2$X, rep(1:4, each = 2))  
}

d1 <- .deconstruct(shuffle_each_sync(three_mix, A, subject, B, C))
d2 <- .deconstruct(shuffle_each_sync(three_mix, A, subject, B, C))
d3 <- .deconstruct(shuffle_each_sync(three_mix, A, subject, B, C))

test_that("synchronized permutation works mixed design", {
  expect_setequal(d1[[1]], d1[[2]])
  expect_setequal(d1[[1]], d1[[3]])
  expect_setequal(d1[[1]], d1[[4]])
  expect_setequal(d2[[1]], d2[[2]])
  expect_setequal(d2[[1]], d2[[3]])
  expect_setequal(d2[[1]], d2[[4]])
  expect_setequal(d3[[1]], d3[[2]])
  expect_setequal(d3[[1]], d3[[3]])
  expect_setequal(d3[[1]], d3[[4]])
})
