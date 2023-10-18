test_that("getWorkingData delivers the correct Data", {
  df <- data.frame(
    Baba=c(rep('C1', 5), rep('C2', 5)),
    Ai=1:10,
    Bi=11:20
  )

  # diagramsToGenerate(c('Ai', 'Bi'))
  expect_equal(getWorkingData(df, 'Ai', 'Bi', 'Baba'),
               data.frame(category= df$Baba, x= df$Ai, y= df$Bi)
  )
  expect_equal(getWorkingData(df, 'Ai', 'Ai/Bi', 'Baba'),
               data.frame(category= df$Baba, x= df$Ai, y= df$Ai/df$Bi)
  )
  expect_equal(getWorkingData(df, 'Ai', 'Bi/Ai', 'Baba'),
               data.frame(category= df$Baba, x= df$Ai, y= df$Bi/df$Ai)
  )
  expect_equal(getWorkingData(df, 'Bi', 'Ai/Bi', 'Baba'),
               data.frame(category= df$Baba, x= df$Bi, y= df$Ai/df$Bi)
  )
  expect_equal(getWorkingData(df, 'Bi', 'Bi/Ai', 'Baba'),
               data.frame(category= df$Baba, x= df$Bi, y= df$Bi/df$Ai)
  )
})
