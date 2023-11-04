#' Title
#'
#' @param dataSrc
#' @param xel
#' @param yel
#' @param categoryCol
#' @param logCoords
#' @param facets
#'
#' @return
#' @export
#'
#' @examples
plotDiagram <- function(dataSrc, xel= "x", yel= "y", categoryCol= "category", logCoords= FALSE, facets= FALSE, ...) {
    dd <- processDiagram(dataSrc = dataSrc, xel = xel, yel = yel, categoryCol = categoryCol, logCoords = logCoords, ...)
    plotForDiagramData(dd, facets = facets)
}



#' Title
#'
#' @param dd
#' @param facets
#' @param showPoints
#' @param showHulls
#' @param showDensity
#' @param pntsAlpha
#'
#' @return
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_density_2d geom_polygon
#'   labs scale_x_log10 scale_y_log10 facet_wrap
#'
#' @examples
plotForDiagramData <-  function(dd, facets= FALSE, showPoints= TRUE, showHulls= TRUE, showDensity= FALSE, pntsAlpha=1) {
  x_lab <- if (!dd$logCoords) dd$xel else sprintf("log(%s)", dd$xel)
  y_lab <- if (!dd$logCoords) dd$yel else sprintf("log(%s)", dd$yel)

  ggplot(mapping=aes(x, y, color= category, shape=category)) +
    list(
      if (showPoints) geom_point(data= dd$catPnts, alpha=pntsAlpha),

      if (showDensity) geom_density_2d(data= dd$catPnts),

      if (showHulls) c(
        geom_polygon(data= dd$catReducedHulls, color='yellow', linewidth= 1, fill=NA),
        geom_polygon(data= dd$catReducedHulls, aes(fill=category), alpha=0.2)
      ),

      labs(
        title = sprintf("%s vs. %s", x_lab, y_lab),
        x= x_lab, y= y_lab,
        subtitle = sprintf('diagram overlap rate: %.2f', rateDiagram(dd$overlapInfo))
      ),

      if (dd$logCoords) scale_x_log10(), scale_y_log10(),
      if (facets) facet_wrap(~category)
    )
}



#' Title
#'
#' @param dd
#' @param facets
#'
#' @return
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_hex labs scale_x_log10 scale_y_log10
#'   facet_wrap
#'
#' @examples
hexPlotForDiagramData <-  function(dd, facets= FALSE) {
  x_lab <- if (!dd$logCoords) dd$xel else sprintf("log(%s)", dd$xel)
  y_lab <- if (!dd$logCoords) dd$yel else sprintf("log(%s)", dd$yel)

  ggplot(mapping=aes(x, y, color= category)) +
    list(
      geom_hex(data= dd$catPnts),

      labs(
        title = sprintf("Hexplot, %s vs. %s", x_lab, y_lab),
        x= x_lab, y= y_lab,
        subtitle = sprintf('diagram overlap rate: %.2f', rateDiagram(dd$overlapInfo))
      ),

      if (dd$logCoords) scale_x_log10(), scale_y_log10(),
      if (facets) facet_wrap(~category)
    )
}



#' Title
#'
#' @param dd
#' @param facets
#'
#' @return
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_segment facet_wrap theme_light
#'
#' @examples
plotTriagles <- function(dd, facets= FALSE){
  x_lab <- if (!dd$logCoords) dd$xel else sprintf("log(%s)", dd$xel)
  y_lab <- if (!dd$logCoords) dd$yel else sprintf("log(%s)", dd$yel)

    ## helper
  trs2trSegs_coords <- function(trs, ps){
    ## build the triangles edges p2 -> p3, p1 -> p3, p1 -> p2 and
    trs.edges <- rbind(trs[, -1], trs[, -2], trs[, -3])
    data.frame(x= ps[trs.edges[, 1], 1], y=ps[trs.edges[, 1], 2], xend= ps[trs.edges[, 2], 1], yend= ps[trs.edges[,2 ], 2])
  }


  catTrsSegs <- lapply(split(dd$catPnts, dd$catPnts$category), function(cp) {
    m <- cbind(cp$x, cp$y)
    trs <- delaunayn(m)
    cbind(category= unique(cp$category), trs2trSegs_coords(trs, m))
  })

  catTrsSegs <- do.call(rbind, c(catTrsSegs, make.row.names= FALSE))


  ggplot(mapping=aes(x, y, color= category, shape=category)) + list(
    geom_segment(data= catTrsSegs, aes(xend=xend, yend=yend), linewidth=.5),
    geom_point(data= dd$catPnts, shape= 21, fill= NA),

    if (facets) facet_wrap(~category),

    theme_light()
  )

}

