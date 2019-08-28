#' Basic functions for shuffling labels
#'
#' These functions reflect alternative ways of shuffling labels so as
#' to respect the constraints of the study design.
#'
#' @name shuffling-functions
#' @examples
#' # Use shuffle for data with a single between subjects factor
#' d <- data.frame(group = rep(c("treatment", "control"), c(6, 6)),
#'                 subject = 1:12)
#' shuffle(d, group)
#' 
#' shuffle_sync(three_way, A, B, C) # shuffle A over B-C
#' shuffle_sync(three_way, B, A, C) # shuffle B over A-C
#'
#' # For multilevel data, nest any within-subject factors
#' # before shuffling. There should be one row per subject.
#' three_nest <- tidyr::nest(three_mix, A)
#' d <- shuffle_sync(three_nest, B, C)
#' # then restore
#' tidyr::unnest(d)
#'
#' \dontrun{
#' # throws error because within-subject factor B introduced
#' # pseudoreplications
#' shuffle_each(two_within, A, subject)
#' }
#' # nest first
#' nested <- tidyr::nest(two_within, B)
#' result <- shuffle_each(nested, A, subject)
#' # then restore
#' tidyr::unnest(result)
#' 
#' shuffle_each_sync(three_mix, A, subject, B) # over B, ignore C
#' shuffle_each_sync(three_mix, A, subject, B, C) # over B:C
NULL

#' @rdname shuffling-functions
#' @param .data Data frame.
#' @param x Unquoted name of factor whose levels are to be shuffled.
#' @export
shuffle <- function(.data, x) {
  xn <- rlang::as_name(rlang::ensym(x))
  data[[xn]] <- sample(data[[xn]])
  data
}

## @details This function is to be applied to single level data
##   only. If you are shuffling between-unit factors in a mixed
##   design, use nesting to collapse the within-unit factors (see
##   example).
## Synchronized relabeling for a between-unit design
##
## Relabel cases, shuffling factor \code{x} synchronized over
## additional factors.

#' @rdname shuffling-functions
#' @inheritParams shuffle
#' @param ... Unquoted names of factors over whose combinations the
#'   shuffling is to be synchronized.
#' @export
shuffle_sync <- function(.data, x, ...) {
  xn <- rlang::as_name(rlang::ensym(x))
  fn <- lapply(rlang::ensyms(...), rlang::as_name)
  f <- lapply(fn, function(.x) .data[[.x]])
  ds <- split(.data[[xn]], f)
  if (!length(unique(sapply(ds, length)) == 1L))
    stop("design not balanced")
  sch <- sample(ds[[1]])
  res1 <- sch
  names(sch) <- ds[[1]]
  sch2 <- replicate(length(ds) - 1L, sample(sch), simplify = FALSE)
  res2 <- mapply(
    function(.x, .y) {
      .z <- .y
      for (.l in unique(names(.x))) {
        .z[.y == .l] <- .x[names(.x) == .l]
      }
      .z
    },
    sch2, ds[-1], SIMPLIFY = FALSE)
  .data[[xn]] <- unsplit(c(list(res1), res2), f)
  .data
}

## Restricted relabeling for a within-unit design
##
## Relabel cases, shuffling factor `x` restricted by `unit`.
##

#' @rdname shuffling-functions
#' @inheritParams shuffle
#' @param unit Unquoted name of variable within the levels of which
#'   the shuffling of 'x' is to be restricted.
#' @export
shuffle_each <- function(.data, x, unit) {
  ## xq <- rlang::enquo(x)
  ## uq <- rlang::enquo(unit)
  if (has_pseudoreplications(.data, {{x}}, {{unit}}))
      stop("Pseudoreplications detected. If the pseudoreplication is due to other within-unit factors, you should nest these factors before using this function.")
  xn <- rlang::as_name(rlang::ensym(x))
  un <- rlang::as_name(rlang::ensym(unit))
  ds <- split(.data[[xn]], .data[[un]])
  res <- lapply(ds, sample)
  .data[[xn]] <- unsplit(res, .data[[un]])
  .data
}

## Restricted, synchronized relabeling for a mixed design
##
## Relabel cases, shuffling factor `x` restricted by `unit` and
## synchronized over additional factors.
##

#' @rdname shuffling-functions
#' @inheritParams shuffle_each
#' @inheritParams shuffle_sync
#' @export
shuffle_each_sync <- function(.data, x, unit, ...) {
  xq <- rlang::enquo(x)
  uq <- rlang::enquo(unit)
  if (has_pseudoreplications(.data, !!xq, !!uq))
      stop("Pseudoreplications detected. If the pseudoreplication is due to other within-unit factors, you should nest these factors before using this function.")
  xn <- rlang::as_name(rlang::ensym(x))
  un <- rlang::as_name(rlang::ensym(unit))
  fn <- lapply(rlang::ensyms(...), rlang::as_name)
  f <- lapply(fn, function(.x) .data[[.x]])

  ## first split into synchronization chunks
  schunks <- split(.data[c(un, xn)], f)

  ## break up each chunk by unit levels
  uchunks <- split(schunks[[1]][[xn]], schunks[[1]][[un]])

  ## determine permutation scheme
  pscheme <- sample(combinat::permn(uchunks[[1]]), length(uchunks), TRUE)
  schemes <- c(list(pscheme),
               replicate(length(schunks) - 1L,
                         sample(pscheme), FALSE))

  ## now apply permutation scheme to each synchronization chunk
  sch2 <- mapply(
    function(.c, .s) {
      .ds <- split(.c[[xn]], .c[[un]])
      if (length(.ds) != length(.s))
        stop("dataset is unbalanced")
      .c[[xn]] <- unsplit(.s, .c[[un]])
      .c
    }, schunks, schemes, SIMPLIFY = FALSE)

  restored <- unsplit(sch2, f)
  .data[[xn]] <- restored[[xn]]
  .data
}
