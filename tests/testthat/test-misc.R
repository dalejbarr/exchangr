context("test-misc")

## test various utility functions
d1 <- data.frame(sub = rep(1:4, each = 4),
                 A = rep(rep(paste0("A", 1:2), each = 2), 4),
                 B = rep(paste0("B", 1:4), each = 4),
                 C = rep(paste0("C", 1:4), times = 4),
                 stringsAsFactors = FALSE)

test_that("pseudoreplications detected", {
  expect_true(has_pseudoreplications(d1, A, sub))
  expect_true(has_pseudoreplications(d1, B, sub))
  expect_false(has_pseudoreplications(d1, C, sub))
})

d1$A <- factor(d1$A)
d2 <- d1
d2$sub <- factor(d2$sub)

test_that("pseudoreplication detection works with factors", {
  expect_true(has_pseudoreplications(d1, A, sub))
  expect_true(has_pseudoreplications(d2, A, sub))
})
