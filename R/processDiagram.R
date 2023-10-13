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
processDiagram <- function(xel, yel, dataSrc, categoryCol= 'category', scaleAxes= TRUE) {
  t <- getWorkingData(xel, yel, dataSrc, categoryCol)

  # get rid of NAs
  t <- na.omit(t)

  if (scaleAxes) {
    r <- (max(t$y) - min(t$y)) / (max(t$x) - min(t$x))
    t$x <- t$x*r
  }

  catsPnts <- split(t, t$category)
  catsHulls <- lapply(catsPnts, function(x) getReducedHull(x[2:3]))
  hs <- do.call(rbind,
          lapply(names(catsHulls), function(c)
            cbind(category= rep(c, nrow(catsHulls[[c]])), x= catsHulls[[c]]['x'], y= catsHulls[[c]]['y'])
          )
  )
  olrs <- overlapRates(catsHulls)

  list(x= xel, y= yel, catsPnts= t, catsHulls= hs, olrs= olrs)
}


#d <- processDiagram(res[1,1], res[1,2], inData, "Gestein")
