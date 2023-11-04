#' Process a discrimination diagram
#'
#' Build and process a discrimination diagram by following these steps:
#'
#' 1) Select elements and a categorical variable from a data set.
#' 2) Remove rows that contain NAs.
#' 3) Divide the data set into categories.
#' 4) Reduce the points set per category to a confidence coefficient using
#'    topological theory and Delaunay triangulation, as proposed by
#'    \insertCite{Han_2020;textual}{didigenr}.
#' 5) The convex hulls for the reduced point sets of the categories are
#'    constructed.
#' 6) The intersecting areas of these hulls are computed.
#'
#' The discrimination diagram can be evaluated, for example, by examining
#' the overlapping regions.
#'
#' @param dataSrc
#' @param xel
#' @param yel
#' @param categoryCol
#' @param logCoords
#' @param ... Will be passed to [getPointsToRemove()]
#'
#' @return
#'
#' @seealso [getPointsToRemove()] for reducing the point set, [getOverlapInfo()]
#'   for the intersecting areas of the hulls
#'
#' @references{
#'   \insertRef{Han_2020}{didigenr}
#' }
#'
#' @export
#'
#' @importFrom stats na.omit
#'
#' @examples
processDiagram <- function(dataSrc, xel='x', yel='y', categoryCol= 'category', logCoords= FALSE, ...) {

  t <- getWorkingData(dataSrc, xel, yel, categoryCol)

  # get rid of NAs
  t <- na.omit(t)

  catsPnts <- split(t, t$category)
  reduHulls <- lapply(catsPnts, function(cp) {
    # copy original coordinates to work with -- e.g. scaling or log transformation
    cp$x_ <- cp$x
    cp$y_ <- cp$y

    # scale axes
    cp$x_  <- cp$x* ( (max(cp$y) - min(cp$y)) / (max(cp$x) - min(cp$x)) )

    if (logCoords) {
      cp$x_ <- log10(cp$x)
      cp$y_ <- log10(cp$y)
    }

    # get and remove points -- reduce points to a confidence range
    cp_redu <- cp[-getPointsToRemove(ps = cbind(cp$x_, cp$y_), ...),  ]

    # delete working copies of coordinates
    # cp_redu$x <- cp_redu$x_
    # cp_redu$y <- cp_redu$y_
    cp_redu$x_ <- NULL
    cp_redu$y_ <- NULL

    ch <- chull(cbind(cp_redu$x, cp_redu$y))
    cp_redu[ch, ]

  })

  # get infos about the calculated reduced hulls and their overlapping
  oi <- getOverlapInfo(lapply(reduHulls, function(rh) cbind(rh[,2], rh[,3])))

  # resemble list of dfs to one df
  hs <- do.call(rbind, c(reduHulls, make.row.names= FALSE))

  list(xel=xel, yel= yel, logCoords= logCoords, catPnts= t, catReducedHulls= hs, overlapInfo= oi)
}
