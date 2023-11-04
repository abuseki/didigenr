#' Title
#'
#' Normalized data set
#'
#' @param dataSrc Queried data frame to get the elements or their ratios
#' @param xel Element or element ratio on the diagram's x axis
#' @param yel Element or element ratio on the diagram's y axis
#' @param categoryCol Name of the column in `dataSrc` holding the categories
#'
#' @return
#'
#' @examples
getWorkingData <- function(dataSrc, xel='x', yel='y', categoryCol="category") {
  with(dataSrc, data.frame(
    category= dataSrc[[categoryCol]],
    x= eval(parse(text=xel)),
    y= eval(parse(text=yel))
  ))
}
