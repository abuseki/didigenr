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

getPlotForDiagram <- function(xel, yel, dataSrc, categoryCol= 'category', scaleAxes= TRUE, facets= FALSE) {
    d <- processDiagram(xel, yel, dataSrc, categoryCol, scaleAxes)

    p <- ggplot(mapping=aes(x, y, color= category, shape=category)) +
    geom_point(data= d$catsPnts) +
    geom_polygon(data= d$catsHulls, color='yellow', size= 1, fill=NA) +
    geom_polygon(data= d$catsHulls, fill=NA) +
    labs(
      title = sprintf("%s vs. %s", d$x, d$y),
      x= d$x, y= d$y,
      subtitle = paste('diagram overlap rate:', round(mean(d$olrs$olRate, na.rm = T), 2))
    )

    if (facets)
        p + facet_wrap(~category)
    else
      p
}
