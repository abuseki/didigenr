test_that("getWorkingData delivers the correct Data", {
  df <- data.frame(
    Baba=c(rep('C1', 5), rep('C2', 5)),
    Ai=1:10,
    Bi=11:20
  )

  # diagramsToGenerate(c('Ai', 'Bi'))
  expect_equal(getWorkingData('Ai', 'Bi', df, 'Baba'),
               data.frame(category= df$Baba, x= df$Ai, y= df$Bi)
  )
  expect_equal(getWorkingData('Ai', 'Ai/Bi', df, 'Baba'),
               data.frame(category= df$Baba, x= df$Ai, y= df$Ai/df$Bi)
  )
  expect_equal(getWorkingData('Ai', 'Bi/Ai', df, 'Baba'),
               data.frame(category= df$Baba, x= df$Ai, y= df$Bi/df$Ai)
  )
  expect_equal(getWorkingData('Bi', 'Ai/Bi', df, 'Baba'),
               data.frame(category= df$Baba, x= df$Bi, y= df$Ai/df$Bi)
  )
  expect_equal(getWorkingData('Bi', 'Bi/Ai', df, 'Baba'),
               data.frame(category= df$Baba, x= df$Bi, y= df$Bi/df$Ai)
  )
})
