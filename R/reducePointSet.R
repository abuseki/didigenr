#library(geometry)


#' Title
#'
#' @param ps
#' @param c
#' @param s
#' @param e
#'
#' @return
#' @export
#'
#' @importFrom geometry delaunayn
#'
#' @examples
reducePointSet_idx <- function(ps, c= .85, s=.95, e=.05) {
  ## helper
  dist <- function(x, y) {
    sqrt(sum((x - y) ^ 2))
  }

  # Indices of points that have to be removed from the given point to fulfill the confidence criterion
  psIdx2remove <- c()

  ## LETS GO
  # number of original points
  N.next <- N.this <- N.start <- nrow(ps)

  while (N.next > N.start * c) {
    N.next <- N.this <- nrow(ps)

    # Delaunay triangulation
    trs <- delaunayn(ps)

    ## resolve triangle coordinates from triangle indices
    trs.coords <- cbind(
      ps[trs[,1], 1:2],
      ps[trs[,2], 1:2],
      ps[trs[,3], 1:2]
    )

    ## find longest edges in triangles
    l <- apply(trs.coords, 1, function(x) max(
      dist(x[1:2], x[3:4]),
      dist(x[1:2], x[5:6]),
      dist(x[3:4], x[5:6])
    ))

    ## l ordered from longest to shortest max. edge, order gives the index of the entry -- that's what we want
    l.ord <- order(l, decreasing = TRUE)

    while(!(N.next < N.this)) {
      ps2rm <- trs[l.ord[1:trunc((1 - s) * nrow(trs))], ]
      N.next <- nrow(ps[-ps2rm, ])

      if (!(N.next < N.this)) {
        # reduction ration was too large, apply adjustment factor
        s <- s-e
        message("Adjusted s to ", s)
      }
    }
    message("N.next/N.start=", N.next/N.start)

    ## remove the points for possible
    ps <- ps[-ps2rm, ]

    # remember the points to remove
    psIdx2remove <- c(psIdx2remove, ps2rm)
  }

  # return indices of points to be removed
  unique(psIdx2remove)
}


#' Title
#'
#' Convenience function that resolves the returned indices of
#' [reducePointSet_idx] to points with coordinates
#'
#' @param ps Set of points as a matrix with x- and y-column
#' @param c
#' @param s
#' @param e
#'
#' @return
#' @export
#'
#' @examples
reducePointSet<- function(ps, c= .85, s=.95, e=.05) {
  ps[-reducePointSet_idx(ps, c, s, e), ]
}



#' Title
#'
#' @param ps
#' @param c
#' @param s
#' @param e
#'
#' @return
#' @export
#'
#' @examples
getReducedHull <- function(ps, c= .85, s=.95, e=.05) {
  ps_redu <- reducePointSet(ps, c, s, e)
  ch <- chull(ps_redu)
  ps_redu[ch, ]
}
