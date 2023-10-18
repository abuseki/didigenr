#' Title
#'
#' @param xel
#' @param yel
#' @param dataSrc
#' @param categoryCol
#'
#' @return
#' @export
#'
#' @examples
rateDiagram <- function(overlapInfo) {
  mean(overlapInfo$olRates, na.rm=T)
}
