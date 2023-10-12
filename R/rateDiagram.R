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
rateDiagram <- function(xel, yel, dataSrc, categoryCol= 'category') {
  t <- getWorkingData(xel, yel, dataSrc, categoryCol)

  # get rid of NAs
  t <- na.omit(t)

  catsPnts <- split(t, t$category)
  # browser()
  catsHulls <- lapply(catsPnts, function(x) getReducedHull(x[2:3]))
  l <- lapply(catsHulls, function(e) cbind(e$x, e$y))
  olrs <- overlapRates(l)
  mean(olrs$olRate)
}
