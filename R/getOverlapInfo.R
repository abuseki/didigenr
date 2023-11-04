#' Title
#'
#' @param regions named list of convex hull coordinates. Names are the
#'   categories and their convex hulls coordinates are presented in x-y-matrix
#'   like data structures. This is what [processDiagram()] returns as
#'   `catReducedHulls`
#'
#' @return List of
#'  * `olAreas`: The overlapping areas of regions
#'  * `olRates`: The overlapping rates per region
#'  * `regionsAreas`: Areas of single regions
#'
#' @seealso [processDiagram()]
#'
#' @export
#'
#' @importFrom geometry intersectn polyarea
#'
#' @examples
getOverlapInfo <- function(regions) {
  regNames <- names(regions)
  nRegs <- length(regNames)

  # matrix of overlap rates
  olrs <- matrix(nrow = nRegs, ncol = nRegs, dimnames = list(regNames, regNames))

  # vector of overlap areas, same datastruc as for overlap rates
  olAreas <- olrs

  # areas of the regions
  regsAreas <- NULL

  for (i in seq_along(regions)) {
    p_i <- regions[[i]]
    area_i <- polyarea(p_i[,1], p_i[,2])
    regsAreas[[regNames[[i]]]] <-  area_i

    for (j in seq_along(regions)[-i]) {
      p_j <- regions[[j]]
      p_ol <- intersectn(p_i, p_j, return.chs = FALSE)$ch$p
      # reorder the points of intersection (clockwise by chull-fctn), intersectn gives other ordering
      ## take care intersectn returns 'NULL' if there is no intersection
      p_ol <-  if(!is.null(p_ol)) p_ol[chull(p_ol), ] else matrix(rep(0, 6), ncol = 2)
      olAreas[i, j] <- polyarea(p_ol[,1], p_ol[,2])
      olrs[i, j] <- olAreas[i, j]/area_i
    }
  }

  # set names of regionsAreas
  list(olAreas= olAreas, olRates= olrs, regionsAreas= regsAreas)
}
