
#' Title
#'
#' * df [`categoryCol`, el1, el2, ..., elN
#' * diagrams will be generated
#' * and all these dias will be rated
#'
#' @param inData data frame with one category column and element value columns
#' @param categoryCol name of the category column in the `inData`
#' @param nCores Number of core to use when rating diagrams in parallel
#'
#' @return data frame holding the x- and y-axis and the overlap rate of this diagram
#' @export
#'
#' @examples
getDiagramsRated <- function(inData, categoryCol= 'category', nCores= 1) {
  # get elements
  ns <- names(inData)
  elems <- ns[which(!(ns %in% categoryCol))]

  # the diagrams to be generated
  dias <- diagramsToGenerate(elems[1:20])
  # or randomly
  dias <- diagramsToGenerate(sample(elems[1:3], 3))

  # register parallel back-end
  message("'getDiagramsRated()' will use ", nCores, " cores!")
  doParallel::registerDoParallel(nCores)

  olrs <- foreach::foreach(i= 1:nrow(dias), .combine = 'c') %dopar% {
    rateDiagram(dias[i, 'x'], dias[i, 'y'], 'inData', categoryCol)
  }

  olrs_ord <- order(olrs)

  data.frame(cbind(x=dias[olrs_ord, 1], y=dias[olrs_ord, 2], olr=olrs[olrs_ord]))
}
