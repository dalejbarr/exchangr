#' Example data from a 3x2x2 between-subjects design
#'
#' A data frame containing labels corresponding to 24 observations
#' from a balanced 3x2x2 full factorial design, with all factors
#' between subjects.
#' 
#' @format A data frame with 24 rows and three variables:
#' \describe{
#'   \item{subject}{Subject identifier.}
#'   \item{A}{Three level factor (A1, A2, A3).}
#'   \item{B}{Two level factor (B1, B2).}
#'   \item{C}{Two level factor (C1, C2).}
#' }
"three_way"

#' Example data from a 2x2 within-subjects design
#'
#' A data frame containing labels corresponding to 24 observations
#' from a balanced 2x2 full factorial design, with both factors within
#' subjects.
#' 
#' @format A data frame with 24 rows and three variables:
#' \describe{
#'   \item{subject}{Subject identifier.}
#'   \item{A}{Two level factor (A1, A2).}
#'   \item{B}{Two level factor (B1, B2).}
#' }
"two_within"

#' Example data from a 3x2x2 mixed design
#'
#' A data frame containing labels corresponding to 24 observations
#' from a balanced 3x2x2 full factorial design, with one within and
#' two between factors.
#' 
#' @format A data frame with 24 rows and four variables:
#' \describe{
#'   \item{subject}{Subject identifier.}
#'   \item{A}{Three level within subjects factor (A1, A2, A3).}
#'   \item{B}{Two level between subjects factor (B1, B2).}
#'   \item{C}{Two level between subjects factor (C1, C2).}
#' }
"three_mix"
