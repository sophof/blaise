context("writing and reading back all types")

expect_type_equal = function(df, column){
  datafile = tempfile(fileext = '.asc')
  blafile = tempfile(fileext = '.bla')

  eval(bquote(expect_silent(write_fwf_blaise(.(df), datafile, blafile))))
  eval(bquote(expect_silent({dfnew = read_fwf_blaise(datafile, blafile)})))
  eval(bquote(expect_equal(.(df)[[.(column)]], dfnew[[.(column)]], tolerance = 1e-7)))
}

test_that("all types get read back identically", {
  df = data.frame(
    int = 1:10,
    factor = sample(LETTERS[1:3], 10, replace = TRUE),
    numeric = runif(10, 1, 10),
    date = sample(seq(as.Date('2010-01-01'), as.Date('2020-01-01'), by = 'day'), 10)
    )
  df$string = as.character(df$factor)

  expect_type_equal(df, 'int')
  expect_type_equal(df, 'factor')
  expect_type_equal(df, 'numeric')
  expect_type_equal(df, 'date')
  expect_type_equal(df, 'string')
})
