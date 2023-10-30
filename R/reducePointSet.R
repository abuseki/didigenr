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
getPointsToRemove <- function(ps, c= .85, s=.95, e=.05) {
  ## helper
  dist <- function(x, y) {
    sqrt(sum((x - y) ^ 2))
  }

  # add row numbers so we can return the rows we removed
  ps <- as.matrix(cbind(ps, seq.int(NROW(ps))))


  ## LETS GO
  # number of original points
  N.next <- N.this <- N.start <- NROW(ps)

  while (N.next > (N.start * c)) {
    N.next <- N.this <- NROW(ps)

    # Delaunay triangulation
    trs <- delaunayn(ps[,1:2])

    ## resolve triangle coordinates from triangle indices
    trs.coords <- cbind(
      ps[trs[,1], 1:2],
      ps[trs[,2], 1:2],
      ps[trs[,3], 1:2]
    )

    ## find longest edges in triangles
    l <- apply(trs.coords, 1, function(tri) max(
      dist(tri[1:2], tri[3:4]),
      dist(tri[1:2], tri[5:6]),
      dist(tri[3:4], tri[5:6])
    ))

    ## l ordered from longest to shortest max. edge, order gives the index of the entry -- that's what we want
    l.ord <- order(l, decreasing = TRUE)

    while(!(N.next < N.this)) {
      ps2rm <- trs[l.ord[seq_len((1 - s) * NROW(trs))], ]
      # message("\t will remove ", NROW(ps2rm), " points")
      N.next <- ifelse(length(ps2rm) != 0L, NROW(ps[-ps2rm, ]), NROW(ps))

      if (!(N.next < N.this)) {
        # reduction ration was too large, apply adjustment factor
        s <- s-e
        message("Adjusted s to ", s)
      }
    }
    # message(sprintf("N.next/N.start= %0.3f%%", N.next/N.start))

    ## remove the points -- indices will remain
    ps <- ps[-ps2rm, ]

  }

  # return the set difference of original row numbers an the one that survived
  # this reduction. These have to be removed
  setdiff(seq.int(N.start), ps[,NCOL(ps)])
}



#' Title
#'
#' Convenience function that resolves the returned indices of
#' [getPointsToRemove] to points with coordinates
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
  ps[-getPointsToRemove(ps, c, s, e), ]
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
