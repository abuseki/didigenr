#' Build and evaluate discrimination diagrams
#'
#' Performs the entire process of creating and evaluating discrimination
#' diagrams as proposed by \insertCite{Han_2020;textual}{didigenr}:
#'
#' 1) All meaningful combinations of elements are generated.
#' 2) All discrimination diagrams are generated and the point sets of the given
#'    categories are reduced to a confidence coefficient.
#' 3) These diagrams are evaluated by the intersection areas of the computed
#'    hulls.
#'
#' @param inData data frame with one category column and element value columns
#' @param categoryCol name of the category column in the `inData`
#' @param logCoords
#' @param nCores Number of core to use when rating diagrams in parallel
#' @param ... will be passed to [processDiagram()]
#'
#' @return data frame holding the x- and y-axis and the overlap rate of this diagram
#'
#' @seealso [processDiagram()] for build a discrimination diagram,
#'   [getOverlapInfo()] and [rateDiagram()] for computing the intersection areas
#'   and evaluation a discrimination diagram by the intersection areas
#'
#' @references{
#'   \insertRef{Han_2020}{didigenr}
#' }
#'
#' @export
#'
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach '%dopar%'
#'
#' @examples
getDiagramsRated <- function(inData, categoryCol= 'category', logCoords= FALSE, nCores= 1, ...) {
  # get elements
  ns <- names(inData)
  elems <- ns[which(!(ns %in% categoryCol))]

  # the diagrams to be generated
  dias <- diagramsToGenerate(elems)
  message("Will process ", nrow(dias), " diagrams. This might take a while...")

  # register parallel back-end
  message("'getDiagramsRated()' will use ", nCores, " cores!")
  registerDoParallel(nCores)

  olrs <- foreach(i= seq_len(nrow(dias)), .combine = 'c') %dopar% {
    dd <- processDiagram(dataSrc = inData, xel = dias[i, 'x'], yel = dias[i, 'y'], categoryCol = categoryCol, logCoords = logCoords, ...)
    rateDiagram(dd$overlapInfo)
  }

  olrs_ord <- order(olrs)

  data.frame(x=dias[olrs_ord, 1], y=dias[olrs_ord, 2], olr=olrs[olrs_ord])
}
