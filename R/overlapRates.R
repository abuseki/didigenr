#' Title
#'
#' TODO put formulas here
#'
#' @param regions named list of regions as polygon coordinates. Coordinates must
#'  be named `x` and `y`
#'
#' @return data.frame with the calculated overlap rates of the given polygons
#' @export
#'
#' @importFrom geometry polyarea intersectn
#'
#' @examples
overlapRates <- function(regions){
  cats <- names(regions)
  olrs <- data.frame(R1=as.character(), R2=as.character(), perReg= as.character(), olRate= as.double())

    for (i in seq_along(regions)) {
    this.poly <- regions[[i]]
    ## TODO use my own version of polyarea?! -- faster!
    this.area <- polyarea(this.poly$x, this.poly$y)

    for (j in seq_along(cats)[-i]) {
      isec.ps <- intersectn(this.poly, regions[[j]], return.chs = FALSE)$ch$p
      # reorder the points of intersection (clockwise by chull-fctn), intersectn gives unusable ordering
      ## take care intersectn return 'NULL' if no there is no intersection
      isec.hull <-  if(!is.null(isec.ps)) isec.ps[chull(isec.ps), ] else matrix(rep(0, 6), ncol = 2)
      olrs <- rbind(olrs, data.frame(R1= cats[i], R2= cats[j], perReg= cats[i], olRate= polyarea(isec.hull[,1], isec.hull[,2])/this.area))
    }
  }

  olrs
}
