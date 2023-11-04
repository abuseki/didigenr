#' Title
#'
#' @param elems list of elements for which the diagrams shall be generated
#'
#' @return data.frame with the diagrams to be generated
#' @export
#'
#' @examples
diagramsToGenerate <- function(elems) {
  # generate the pairs and the elemsSet we'll work with
  pairs <- c()
  for (i in seq_len(length(elems))) {
    for(j in elems[-i]) {
      pairs <- c(pairs, paste0(elems[i], '/', j))
    }
  }
  elemsSet <- c(elems, pairs)

  # diagrams to generate, combn returns a matrix which is an array
  diagrams <- combn(elemsSet, 2)


  # Filter inverse pairs like a/b vs b/a
  ## we need to find two consecutive elements containing '/'
  ## we traverse the diagram matrix in colums
  cols2rm <- which(apply(diagrams, 2, function(i) {
          a <- unlist(strsplit(i[1], '/', fixed = TRUE))
          b <- unlist(strsplit(i[2], '/', fixed = TRUE))

          (a[1] == b[2] && a[2] == b[1])
  }))

  diagrams <- diagrams[, -cols2rm]

  # convert this to a data.frame
  data.frame(cbind(x= diagrams[1, ], y=diagrams[2, ]))
}



#' Title
#'
#' Simple function to calculate the number of element (element ratio) pairs for
#' a given number of elements. This is also the number of diagrams that will
#' have to be processed.
#'
#' @param nElems number of elements for which the number of diagrams will be
#'   calculated
#'
#' @return
#' @export
#'
#'
#' @examples
nDiagsForElems <- function(nElems) {
  nElemRatios <- nElems*(nElems-1)
  nTotalElems <- nElemRatios + nElems

  # subtract inverse pairs, e.g A/B-B/A
  choose(nTotalElems, 2) - nElemRatios/2
}
