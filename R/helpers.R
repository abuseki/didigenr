

################################################################################
trsIdx2trsSegsCoords <- function(trs, ps){
  ## build the triangles edges p2 -> p3, p1 -> p3, p1 -> p2 and
  trs.edges <- rbind(trs[, -1], trs[, -2], trs[, -3])
  data.frame(x= ps[trs.edges[, 1], 1], y=ps[trs.edges[, 1], 2], xend= ps[trs.edges[, 2], 1], yend= ps[trs.edges[,2 ], 2])
}
################################################################################


#' Title
#'
#' @param dataSrc Queried data frame to get the elements or their ratios
#' @param xel Element or element ratio on the diagram's x axis
#' @param yel Element or element ratio on the diagram's y axis
#' @param categoryCol Name of the column in `dataSrc` holding the categories
#'
#' @return
#' @export
#'
#' @examples
getWorkingData <- function(dataSrc, xel='x', yel='y', categoryCol="category") {
  with(dataSrc, data.frame(
    category= dataSrc[[categoryCol]],
    x= eval(parse(text=xel)),
    y= eval(parse(text=yel))
  ))
}
