

################################################################################
trsIdx2trsSegsCoords <- function(trs, ps){
  ## build the triangles edges p2 -> p3, p1 -> p3, p1 -> p2 and
  trs.edges <- rbind(trs[, -1], trs[, -2], trs[, -3])
  data.frame(x= ps[trs.edges[, 1], 1], y=ps[trs.edges[, 1], 2], xend= ps[trs.edges[, 2], 1], yend= ps[trs.edges[,2 ], 2])
}
################################################################################


################################################################################
# TODO move to own file e.g. diaPlotters.R
################################################################################
# plotCatsAndHulls <- function(ps, hs, xEl, yEl, facets= FALSE) {
#   library(ggplot2)
#   if (facets) library(patchwork)
#
#   p <- ggplot(data = ps, mapping= aes(x=x, y=y, color= category)) +
#     geom_point(aes(shape= category)) +
#     geom_polygon(data= hs, fill=NA) +
#     labs(title = sprintf("%s vs. %s", xEl, yEl), x= xEl, y= yEl)
#
#
#   if (facets)
#     (p + facet_wrap(~category)) / p
#   else
#     p
# }
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


################################################################################
#' Title
#'
#' @param xel Element or element ratio on the diagram's x axis
#' @param yel Element or element ratio on the diagram's y axis
#' @param dataSrc Queried data frame to get the elements or their ratios
#' @param categoryCol Name of the column in `dataSrc` holding the categories
#'
#' @return
#'
#' @examples
build_getElemDataString <- function(xel, yel, dataSrc, categoryCol= "category") {
  xs <- strsplit(xel, '/', fixed = TRUE)[[1]]
  ys <- strsplit(yel, '/', fixed = TRUE)[[1]]

  # browser()
  paste0("data.frame(category= ", dataSrc, "[['", categoryCol, "']], ",
         "x= ", dataSrc,"[['", xs[1], "']]",
         if(!is.na(xs[2])) paste0("/", dataSrc,"[['", xs[2], "']]"),
         ", ",
         "y= ", dataSrc,"[['", ys[1], "']]",
         if(!is.na(ys[2])) paste0("/", dataSrc,"[['", ys[2], "']]"),
  ")")
}
################################################################################
