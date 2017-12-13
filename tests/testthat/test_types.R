context("writing and reading back all types")

expect_type_equal = function(df, column){
  datafile = tempfile(fileext = '.asc')
  blafile = tempfile(fileext = '.bla')

  eval(bquote(expect_silent(write_fwf_blaise(.(df), datafile, blafile))))
  eval(bquote(expect_silent({dfnew = read_fwf_blaise(datafile, blafile)})))
  eval(bquote(expect_equal(.(df)[[.(column)]], dfnew[[.(column)]], tolerance = 1e-7)))
}

test_that("all types get read back identically for a dataframe", {
  df = data.frame(
    int = 1:10,
    factor = as.factor(LETTERS[1:10]),
    numeric = seq(1.1, 10.1, by = 1),
    date = seq(as.Date('2010-01-01'), as.Date('2010-01-10'), by = 'day')
    )
  df$string = as.character(df$factor)

  expect_type_equal(df, 'int')
  expect_type_equal(df, 'factor')
  expect_type_equal(df, 'numeric')
  expect_type_equal(df, 'date')
  expect_type_equal(df, 'string')
})

test_that("all types get read back identically for a tibble", {
  df = data.frame(
    int = 1:10,
    factor = sample(LETTERS[1:3], 10, replace = TRUE),
    numeric = runif(10, 1, 10),
    date = sample(seq(as.Date('2010-01-01'), as.Date('2020-01-01'), by = 'day'), 10)
  )
  df$string = as.character(df$factor)
  df = dplyr::as_tibble(df)

  expect_type_equal(df, 'int')
  expect_type_equal(df, 'factor')
  expect_type_equal(df, 'numeric')
  expect_type_equal(df, 'date')
  expect_type_equal(df, 'string')
})

test_that("multiple factors can be written and read", {
  df = data.frame(
    letters = as.factor(LETTERS[1:10]),
    date = as.factor(seq(as.Date('2010-01-01'), as.Date('2010-01-10'), by = 'day'))
  )

  expect_type_equal(df, 'letters')
  expect_type_equal(df, 'date')
})
