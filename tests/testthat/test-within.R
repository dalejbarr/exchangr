context("test-within")

dfac <- tibble::tibble(
                  subject = rep(1:4, each = 3),
                  A = rep(rep(paste0("A", 1:3), 4)),
                  B = rep(paste0("B", rep(1:2, each = 3)), 2),
                  C = rep(paste0("C", 1:4), 3),
                  D = rep("D1", 12))

dfac_scr <- dfac[c(1:2, 4:5, 7:8, 10:11, 3, 6, 9, 12), ]

d1 <- shuffle_each(dfac, A, subject)
d2 <- shuffle_each(dfac, A, subject)
d3 <- shuffle_each(dfac, A, subject)

d4 <- shuffle_each(dfac_scr, A, subject)
d5 <- shuffle_each(dfac_scr, A, subject)
d6 <- shuffle_each(dfac_scr, A, subject)

dx <- data.frame(sub = rep(1:4, each = 4),
                 A = rep(rep(paste0("A", 1:2), each = 2), 4),
                 B = rep(paste0("B", 1:4), each = 4),
                 C = rep(paste0("C", 1:4), times = 4),
                 stringsAsFactors = FALSE)

dn <- tidyr::nest(two_within, B)

dn2 <- dn
dn2$A <- factor(dn2$A)

test_that("four factor", {
  expect_true(is_within(dfac, A, subject))
  expect_false(is_within(dfac, B, subject))
  expect_error(is_within(dfac, D, subject))
})

test_that("shuffling occurs within unit", {
  expect_true(is_within(d1, A, subject))
  expect_true(is_within(d2, A, subject))
  expect_true(is_within(d3, A, subject))
  expect_true(is_within(d4, A, subject))
  expect_true(is_within(d5, A, subject))
  expect_true(is_within(d6, A, subject))
})

test_that("shuffling fails with pseudoreplications", {
  expect_error(shuffle_each(dx, A, sub))
  expect_error(shuffle_each_sync(dx, A, sub, B))
  expect_error(shuffle_each(two_within, A, subject))
  expect_is(shuffle_each(dn, A, subject), "data.frame")
})

test_that("shuffling works with factors", {
  expect_true(is.data.frame(shuffle_each(dn2, A, subject)))
})
