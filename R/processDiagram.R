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

  catsPnts <- split(t, t$category)
  catsHulls <- lapply(catsPnts, function(cp) {
    if (scaleAxes) {
      cp$x_ <- cp$x
      cp$x  <- cp$x* ( (max(cp$y) - min(cp$y)) / (max(cp$x) - min(cp$x)) )
    }
    cp_redu <- cp[-getPointsToRemove(cbind(cp$x, cp$y)), ]

    cp_redu <- data.frame(category= cp_redu$category, x= cp_redu$x_, y= cp_redu$y)

    ch <- chull(cbind(cp_redu$x, cp_redu$y))
    cp_redu[ch, ]

  })

  # resemble list of dfs to one df
  hs <- do.call(rbind, c(catsHulls, make.row.names= FALSE))

  # calculate the overlap rate of the (reduced) hulls
  olrs <- overlapRates(catsHulls)

  list(x= xel, y= yel, catsPnts= t, catsHulls= hs, olrs= olrs)
}
