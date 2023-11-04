#' Title
#'
#' @param ws
#' @param split
#'
#' @return
#'
#' @examples
wordcounting <- function(ws, split='/') {
  # lapply(ws, function(w) {strsplit(w, split) })

  unlist(
  lapply(ws, function(w) {
    unlist(strsplit(w, split))
  })
  )
}
#
# d <- geodisctoponator::diagramsToGenerate(c('A', 'B'))
# unique(wordcounting(c(d[,1],d[,2])))
#
# res <- readxl::read_excel('../../../Data/PegStat__processingDataSet_cleanedMjs__98__res.xlsx')
# d <- res[which(res$olr<=.1),]
# wc <- wordcounting(c(d[,1],d[,2]))
# t <- table(wc)
# t[order(t, decreasing = TRUE)]
# unique(wc)
