#' Title
#'
#' @param xel
#' @param yel
#' @param dataSrc
#' @param facets
#'
#' @return
#' @export
#'
#' @examples

getPlotForDiagram <- function(dataSrc, xel= "x", yel= "y", categoryCol= "category", scaleAxes= TRUE, facets= FALSE) {
    dd <- processDiagram(dataSrc, xel, yel, categoryCol, scaleAxes)

    p <- ggplot(mapping=aes(x, y, color= category, shape=category)) +
    geom_point(data= dd$catPnts) +
    geom_polygon(data= dd$catReducedHulls, color='yellow', size= 1, fill=NA) +
    geom_polygon(data= dd$catReducedHulls, fill=NA) +
    labs(
      title = sprintf("%s vs. %s", xel, yel),
      x= xel, y= yel,
      subtitle = paste('diagram overlap rate:', round(rateDiagram(dd$overlapInfo), 2))
    )

    if (facets)
        p + facet_wrap(~category)
    else
      p
}
