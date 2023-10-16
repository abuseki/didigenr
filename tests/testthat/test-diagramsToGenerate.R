test_that("nDiagsForElems gives the correct amount of diagrams", {
  expect_equal(nDiagsForElems(2), 5)
  expect_equal(nDiagsForElems(3), 33)
  expect_equal(nDiagsForElems((26)), 227825)
})

test_that("diagramsToGenerate gives the correct amount of diagrams", {
  ## TODO remove random, this is while devl to the test do not take to long
  nes <- 1+sample(20, 3)
  expect_equal(
    sapply(nes, function(i) nrow(diagramsToGenerate(1:i))),
    sapply(nes, function(i) nDiagsForElems(i))
  )
})


test_that("diagramsToGenerate gives no inverse pair", {
  df <- diagramsToGenerate(1:3)
  df <- df[sapply(seq_len(nrow(df)), function(i) all(grepl("/", df[i,], fixed = T))),]
  df <- df[sapply(seq_len(nrow(df)), function(i) {
            a= strsplit(df[i, 1], '/', fixed= TRUE)
            b= strsplit(df[i, 2], '/', fixed= TRUE)

            a[[1]][1] == b[[1]][2] && a[[1]][2] == b[[1]][1]
  }),]

  expect_equal(nrow(df), 0L)

  #
  # Note:
  #   find all/any cells having '/' in df:
  #   dd <- diagramsToGenerate(names(inData[15:18]))
  #   dd[sapply(1:nrow(dd), function(i) all(grepl("/", dd[i,], fixed = T))),]
  #
  # dx[sapply(1:nrow(dx), function(i) {
  #   a= strsplit(dx[i, 1], '/', fixed= TRUE)
  #   b= strsplit(dx[i, 2], '/', fixed= TRUE)
  #
  #   a[[1]][1] == b[[1]][2] && a[[1]][2] == b[[1]][1]
  #
  # }),]

})
