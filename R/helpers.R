

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
build_selectElemString <- function(xel, yel) {
  xs <- strsplit(xel, '/', fixed = TRUE)[[1]]
  ys <- strsplit(yel, '/', fixed = TRUE)[[1]]

  # browser()
  paste0("data.frame(category= inData[[categoryCol]], ",
         "x= inData[['", xs[1], "']]",
         if(!is.na(xs[2])) paste0("/inData[['", xs[2], "']]"),
         ", ",
         "y= inData[['", ys[1], "']]",
         if(!is.na(ys[2])) paste0("/inData[['", ys[2], "']]"),
  ")")
}
################################################################################
