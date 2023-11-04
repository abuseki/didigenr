test_that("getWorkingData delivers the correct Data", {
  df <- data.frame(
    Baba=c(rep('C1', 5), rep('C2', 5)),
    Ai=1:10,
    Bu=11:20
  )

  expect_equal(getWorkingData(df, 'Ai', 'Bu', 'Baba'),
               data.frame(category= df$Baba, x= df$Ai, y= df$Bu)
  )
  expect_equal(getWorkingData(df, 'Ai', 'Ai/Bu', 'Baba'),
               data.frame(category= df$Baba, x= df$Ai, y= df$Ai/df$Bu)
  )
  expect_equal(getWorkingData(df, 'Ai', 'Bu/Ai', 'Baba'),
               data.frame(category= df$Baba, x= df$Ai, y= df$Bu/df$Ai)
  )
  expect_equal(getWorkingData(df, 'Bu', 'Ai/Bu', 'Baba'),
               data.frame(category= df$Baba, x= df$Bu, y= df$Ai/df$Bu)
  )
  expect_equal(getWorkingData(df, 'Bu', 'Bu/Ai', 'Baba'),
               data.frame(category= df$Baba, x= df$Bu, y= df$Bu/df$Ai)
  )

  dx <- getWorkingData(df, 'log10(Ai)', 'Bu', 'Baba')
  expect_equal(cbind(dx$x, dx$y), cbind(log10(df$Ai), df$Bu))

  dx <- getWorkingData(df, 'log10(Ai)', 'log10(Bu)', 'Baba')
  expect_equal(cbind(dx$x, dx$y), cbind(log10(df$Ai), log10(df$Bu)))

  dx <- getWorkingData(df, 'Ai', 'log10(Ai/Bu)', 'Baba')
  expect_equal(cbind(dx$x, dx$y), cbind(df$Ai, log10(df$Ai/df$Bu)))

  dx <- getWorkingData(df, 'log10(Bu/Ai)', 'log10(Ai/Bu)', 'Baba')
  expect_equal(cbind(dx$x, dx$y), cbind(log10(df$Bu/df$Ai), log10(df$Ai/df$Bu)))

})
