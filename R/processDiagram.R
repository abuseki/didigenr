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
processDiagram <- function(dataSrc, xel='x', yel='y', categoryCol= 'category', scaleAxes= TRUE) {
  t <- getWorkingData(dataSrc, xel, yel, categoryCol)

  # get rid of NAs
  t <- na.omit(t)

  catsPnts <- split(t, t$category)
  reduHulls <- lapply(catsPnts, function(cp) {
    if (scaleAxes) {
      # save original coordinates, before scaling
      cp$x_ <- cp$x
      # scale axes
      cp$x  <- cp$x* ( (max(cp$y) - min(cp$y)) / (max(cp$x) - min(cp$x)) )
    }
    cp_redu <- cp[-getPointsToRemove(cbind(cp$x, cp$y)), ]

    # restore original coordinates
    cp_redu$x <- cp_redu$x_
    cp_redu$x_ <- NULL


    ch <- chull(cbind(cp_redu$x, cp_redu$y))
    cp_redu[ch, ]

  })

  # get infos about the calculated reduced hulls and their overlapping
  oi <- getOverlapInfo(lapply(reduHulls, function(rh) cbind(rh[,2], rh[,3])))

  # resemble list of dfs to one df
  hs <- do.call(rbind, c(reduHulls, make.row.names= FALSE))

  list(catPnts= t, catReducedHulls= hs, overlapInfo= oi)
}
