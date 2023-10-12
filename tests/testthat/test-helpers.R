test_that("build_getElemDataString does the job correctly", {
  expect_equal(build_getElemDataString("A", "B", "inData"),
    "data.frame(category= inData[['category']], x= inData[['A']], y= inData[['B']])"
  )
  expect_equal(build_getElemDataString("A", "B", "df", "cat"),
               "data.frame(category= df[['cat']], x= df[['A']], y= df[['B']])"
  )
  expect_equal(build_getElemDataString("A/B", "B", "inData"),
               "data.frame(category= inData[['category']], x= inData[['A']]/inData[['B']], y= inData[['B']])"
  )
  expect_equal(build_getElemDataString("A", "B/C", "inData"),
               "data.frame(category= inData[['category']], x= inData[['A']], y= inData[['B']]/inData[['C']])"
  )
  expect_equal(build_getElemDataString("A/B", "B/A", "inData"),
               "data.frame(category= inData[['category']], x= inData[['A']]/inData[['B']], y= inData[['B']]/inData[['A']])"
  )
})
