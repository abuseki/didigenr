

################################################################################
trsIdx2trsSegsCoords <- function(trs, ps){
  ## build the triangles edges p2 -> p3, p1 -> p3, p1 -> p2 and
  trs.edges <- rbind(trs[, -1], trs[, -2], trs[, -3])
  data.frame(x= ps[trs.edges[, 1], 1], y=ps[trs.edges[, 1], 2], xend= ps[trs.edges[, 2], 1], yend= ps[trs.edges[,2 ], 2])
}
################################################################################


################################################################################
# TODO implement without dplyr, tidyr
################################################################################
# prepareDataSet <- function(df, categoryCol, xEl, yEl) {
#   library(dplyr)
#   library(tidyr)
#
#   df %>%
#     select(all_of(c(categoryCol, xEl, yEl))) %>%
#     drop_na() %>%
#     ## rename category and coordinate columns, so the code is easier
#     rename(category= `categoryCol`, x= xEl, y= yEl)
# }
################################################################################


#' Title
#'
#' @param xel Element or element ratio on the diagram's x axis
#' @param yel Element or element ratio on the diagram's y axis
#' @param dataSrc Queried data frame to get the elements or their ratios
#' @param categoryCol Name of the column in `dataSrc` holding the categories
#'
#' @return
#' @export
#'
#' @examples
getWorkingData <- function(xel, yel, dataSrc, categoryCol="category") {
  with(dataSrc, data.frame(
    category= dataSrc[[categoryCol]],
    x= eval(parse(text=xel)),
    y= eval(parse(text=yel))
  ))
}
