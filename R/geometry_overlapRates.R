#' Title
#'
#' TODO put formulaes here
#'
#' @param regions named list of regions as polygon coordinates. Coordinates must
#'  be named `x` and `y`
#'
#' @return data.frame with the calculated overlap rates of the given polygons
#' @export
#'
#' @examples
overlapRates <- function(regions){
  # catHulls[catHulls=="Pegmatit",2:3]
  #
  # lll <- split(catHulls, catHulls$category)
  # regions <- lapply(lll, function(e) cbind(e$x, e$y))
  cats <- names(regions)
  nCats <- length(cats)

  olrs <- data.frame(R1=as.character(), R2=as.character(), perReg= as.character(), olRate= as.double())
  for (i in seq_along(regions)) {
    this.poly <- regions[[i]]
    ## TODO use ny own version of polyarea?! -- faster!
    this.area <- geometry::polyarea(this.poly[,1], this.poly[,2])

    for (j in seq.int(1, nCats)[-i]) {
      isec.ps <- geometry::intersectn(this.poly, regions[[j]], return.chs = FALSE)$ch$p
      # reorder the points of intersection (clockwise by chull-fctn), intersectn gives unusual and unuseable ordering
      ## take care intersectn return 'NULL' if no there is no intersection
      isec.hull <-  if(!is.null(isec.ps)) isec.ps[chull(isec.ps), ] else matrix(rep(0, 6), ncol = 2)
      olrs <- rbind(olrs, data.frame(R1= cats[i], R2= cats[j], perReg= cats[i], olRate= geometry::polyarea(isec.hull[,1], isec.hull[,2])/this.area))
    }
  }

  olrs
}
