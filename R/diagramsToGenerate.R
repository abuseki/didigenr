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
  #num_pairs <- choose(length(elemsSet), 2)

  # diagrams to generate, combn returns a matrix which is an array
  diagrams <- combn(elemsSet, 2)

  ## safety check length if diagrams need to be an even number
  if(length(diagrams) %% 2 != 0) {
    stop("length of diagrams is not an even number")
  }

  # Filter inverse pairs like a/b vs b/a
  idx2remove <- c()
  ## we need to find two consecutive elements containing '/'
  ## 1st element must have an odd index, so we just take
  ## every second index starting at one.
  for (i in seq.int(1, length(diagrams), 2)) {
    if (all(grepl('/', c(diagrams[i], diagrams[i+1])))) {
      # fixed= TRUE we do not neeed an regexpr, speeds up things a little
      a <- strsplit(diagrams[i], '/', fixed = TRUE)
      b <- strsplit(diagrams[i+1], '/', fixed = TRUE)
      if (
        a[[1]][1] == b[[1]][2] &&
        a[[1]][2] == b[[1]][1]
      ) {
        idx2remove <- c(idx2remove, i, i+1)
      }
    }
  }
  # remove inverse pairs, the are the diagrams to generate
  d2g <- diagrams[-idx2remove]

  # convert this to a data.frame
  ndiags <- length(d2g)
  odds <- seq(1, ndiags, 2)
  evens <- seq(2, ndiags, 2)

  data.frame(cbind(x= diagrams[odds], y=diagrams[evens]))
}
