#' Check whether levels of a factor are within- or between- unit.
#'
#' @param data Unquoted name of a data frame.
#' @param x Unquoted name of variable to check.
#' @param unit Unquoted name of sampling unit factor.
#' @details First checks whether the factor \code{x} is between unit
#'   by testing that each sampling unit has only a single level. If
#'   not, then the factor is assumed to be within. The user is warned
#'   if some units are missing levels. This behavior can be turned off
#'   by wrapping the call in \code{suppressWarnings()}.
#' @examples
#' d <- data.frame(
#'   subject = rep(1:2, each = 2),
#'   A = rep(paste0("A", 1:2), 2),
#'   B = rep(paste0("B", 1:2), each = 2))
#' @export
is_within <- function(data, x, unit) {
  fc <- rlang::as_name(rlang::ensym(x))
  nm <- rlang::as_name(rlang::ensym(unit))
  lvls <- if (is.factor(data[[fc]])) levels(data[[fc]]) else unique(data[[fc]])
  if (length(lvls) == 1L)
    stop("factor '", fc, "' has only one level")
  units <- sapply(split(data[[fc]], data[[nm]]), unique, simplify = FALSE)

  ## test whether all levels of the factor are present for each unit
  lvec <- sapply(units, function(.x) length(setdiff(lvls, .x)) == 0L)
  lvec_len <- sapply(units, length)
  
  if (all(lvec)) {
    ## is within
    TRUE
  } else if (any(lvec_len > 1L)) {
    stop("factor '", fc, "' was partially within and partially between")
  } else {      
    FALSE
  }
}

#' Does the dataset have pseudoreplications for a given factor?
#'
#' @param data A data frame.
#' @param x Unquoted name of variable to check (must be character or factor).
#' @param unit Unquoted name of variable identifying individual sampling units. 
#' @details Tests whether there are multiple observations per level of
#'   factor \code{x} per unit.
#' @export
has_pseudoreplications <- function(data, x, unit) {
  ## strategy: split up the dataset by unit sort the factor levels,
  ## and use run-length encoding to count repetitions.
  ds <- split(data, dplyr::pull(data, {{unit}}))
  res <- sapply(ds, function(.x) {
    any(rle(sort(as.character(dplyr::pull(.x, {{x}}))))$lengths != 1L)
  })
  any(res)
}

## Fill in missing rows for omitted factor levels.
##
## @param data Unquoted name of a data frame.
## @param .spec Sampling design specification; \code{NULL} for single-level data.
## @param silent Whether to show a message describing the result of the operation.
## @seealso \code{\link{specify}}
## rectify <- function(data, .spec = NULL, silent = FALSE) {
## TODO: tidyr::crossing
## TODO: dplyr::left_join?
## }
