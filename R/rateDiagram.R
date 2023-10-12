#' Title
#'
#' @param xel
#' @param yel
#' @param ds
#' @param categoryCol
#'
#' @return
#' @export
#'
#' @importFrom stats na.omit
#' @examples
rateDiagram <- function(xel, yel, ds, categoryCol= 'category') {
  s <- build_getElemDataString(xel, yel, ds, categoryCol)
  # print(s)
  t <- eval(parse(text= s))

  # get rid of NAs
  t <- na.omit(t)

  catsPnts <- split(t, t$category)
  # browser()
  catsHulls <- lapply(catsPnts, function(x) getReducedHull(x[2:3]))
  l <- lapply(catsHulls, function(e) cbind(e$x, e$y))
  olrs <- overlapRates(l)
  mean(olrs$olRate)
}
