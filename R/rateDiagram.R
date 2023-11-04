#' Title
#'
#' @param overlapInfo
#'
#' @return
#' @export
#'
#' @examples
rateDiagram <- function(overlapInfo) {
  mean(overlapInfo$olRates, na.rm=T)
}
