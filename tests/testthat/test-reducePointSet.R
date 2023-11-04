test_that("getPointsToRemove gives the correct amount of points to remove", {
  ps <- matrix(rnorm(200), ncol = 2)

  # trivial cases
  expect_equal(getPointsToRemove(ps, c=1), integer(0))
  expect_equal(getPointsToRemove(ps, c=0), seq.int(NROW(ps)))

  # cases between extremes
  confidence <- seq(0.1, .85, by=.15)
  expected <- c( 90, 75, 60, 48, 30, 15)
  for (i in seq_along(confidence)) {
    psr <- getPointsToRemove(ps, confCoef = confidence[i], reduRat = 1,  adjuFact = .01)
    expect_equal(NROW(psr), expected[i])
  }
})


test_that("getPointsToRemove removes correct points", {
  set.seed(777)
  ps <- data.frame(
    x= rnorm(1000, 100, 0.5),
    y= rnorm(1000, 100, 0.5)
  )
  rx <- range(ps$x)
  ry <- range(ps$y)
  nps <- nrow(ps)

  distToHeap <- 90
  ps <- rbind(ps, cbind(x= rnorm(3, rx - distToHeap), y=rnorm(3, ry - distToHeap)))
  ps <- rbind(ps, cbind(x= rnorm(3, rx - distToHeap), y=rnorm(3, ry + distToHeap)))
  ps <- rbind(ps, cbind(x= rnorm(3, rx + distToHeap), y=rnorm(3, ry + distToHeap)))
  ps <- rbind(ps, cbind(x= rnorm(3, rx + distToHeap), y=rnorm(3, ry - distToHeap)))

  ptp <- getPointsToRemove(ps, confCoef = .99, reduRat = .99)
  psr <- ps[-ptp,]

  # all erroneous points removed?
  expect_contains(ptp, seq.int(12)+nps)

  # are survived points in correct range
  expect_true(all(with(psr, rx[1] <= x & x <= rx[2])))
  expect_true(all(with(psr, ry[1] <= y & y <= ry[2])))

})
