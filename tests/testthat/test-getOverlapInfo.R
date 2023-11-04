test_that("getOverlapInfo gives the correct overlap rates", {


  oi <- getOverlapInfo(readRDS("../testdata/polys__ol_0.rds"))
  expect_equal(mean(oi$olRates, na.rm=TRUE), 0)
  expect_equal(oi$regionsAreas$A, 1)

  oi <- getOverlapInfo(readRDS("../testdata/polys__ol_50.rds"))
  expect_equal(mean(oi$olRates, na.rm=TRUE), 0.5)

  oi <- getOverlapInfo(readRDS("../testdata/polys__3nested.rds"))
  expect_equal(oi$olRates, matrix(c(NA, 4/16, 1/16,   4/4, NA, 1/4,  1, 1, NA), nrow= 3, ncol=3, byrow = TRUE, dimnames= list(LETTERS[1:3], LETTERS[1:3])))
  expect_equal(matrix(oi$olAreas, ncol = 3), matrix(c(NA, 4, 1,   4, NA, 1,  1, 1, NA), ncol=3, byrow = TRUE))
  expect_equal(oi$regionsAreas, list(A=16, B=4, C=1))
})
