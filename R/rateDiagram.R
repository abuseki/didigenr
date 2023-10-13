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
#' @importFrom stats na.omit
#' @examples
rateDiagram <- function(xel, yel, dataSrc, categoryCol= 'category', scaleAxes= TRUE) {
  d <- processDiagram(xel, yel, dataSrc, categoryCol, scaleAxes)
  mean(d$olrs$olRate, na.rm = T)
}
