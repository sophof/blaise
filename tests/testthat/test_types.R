context("writing and reading back all types")

makeblafile = function(model){
  blafile = tempfile('testbla', fileext = '.bla')
  writeLines(model, con = blafile)
  return(blafile)
}

makedatafile = function(data){
  datafile = tempfile('testdata', fileext = '.asc')
  charToRaw(data)
  readr::write_file(data, datafile)
  return(datafile)
}

expect_type_equal = function(df, column){
  datafile = tempfile(fileext = '.asc')
  blafile = tempfile(fileext = '.bla')

  eval(bquote(expect_silent(write_fwf_blaise(.(df), datafile, blafile, model_name = 'test'))))
  eval(bquote(expect_silent({dfnew = read_fwf_blaise(datafile, blafile)})))
  eval(bquote(expect_equal(.(df)[[.(column)]], dfnew[[.(column)]], tolerance = 10^(-1 * getOption('digits')))))
}

test_that("all types get read back identically for a dataframe", {
  df = data.frame(
    int = 1:10,
    factor = as.factor(rep(c('male','female'), 5)),
    numeric = seq(1.1111111111, 10.1111111111, by = 1),
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

test_that("all types get read back identically with NA", {
  df = data.frame(
    int = 1:10,
    factor = as.factor(rep(c('male','female'), 5)),
    numeric = seq(1.1111111111, 10.1111111111, by = 1),
    date = seq(as.Date('2010-01-01'), as.Date('2010-01-10'), by = 'day')
  )
  df$string = as.character(df$factor)
  df[5,] = NA

  expect_type_equal(df, 'int')
  expect_type_equal(df, 'factor')
  expect_type_equal(df, 'numeric')
  expect_type_equal(df, 'date')
  expect_type_equal(df, 'string')
})

test_that("single width numeric", {
  df = data.frame(
    numeric = seq(1, 9, by = 1.)
  )
  df[5,] = NA
  expect_type_equal(df, 'numeric')
})

test_that("booleans are converted to int with a message", {
  df = data.frame(
    bool = sample(c(T,F), 10, replace = TRUE)
  )
  df[5,] = NA
  datafile = tempfile(fileext = '.asc')
  blafile = tempfile(fileext = '.bla')
  expect_message(write_fwf_blaise(df, datafile, blafile))
  expect_silent({dfnew = read_fwf_blaise(datafile, blafile)})
  expect_equal(as.integer(df[['bool']]), dfnew[['bool']])
})

test_that("numbered enums write out the same numbers as are read, including NA", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : (Male (1), Female (2), Unknown (9))
  B     : (M(1),F(2),X(10))
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "1 1\r\n2 2\r\n910\r\n"
  datafile = makedatafile(data)
  output = tempfile(fileext = '.asc')
  outputbla = tempfile(fileext = '.bla')

  expect_silent({df = read_fwf_blaise(datafile, blafile)})
  expect_silent(write_fwf_blaise(df, output, outputbla))
  outdata = readr::read_file(output)
  bla = read_model(outputbla)
  expect_equal(outdata, data)
  expect_equivalent(model_widths(bla), c(1, 2))

  data = "1  \r\n2 2\r\n 10\r\n"
  datafile = makedatafile(data)
  output = tempfile(fileext = '.asc')
  outputbla = tempfile(fileext = '.bla')

  expect_silent({df = read_fwf_blaise(datafile, blafile)})
  expect_silent(write_fwf_blaise(df, output, outputbla))
  outdata = readr::read_file(output)
  bla = read_model(outputbla)
  expect_equal(outdata, data)
  expect_equivalent(model_widths(bla), c(1, 2))
})

test_that("Reals with borderline 'small' values", {
  df = data.frame(
    one = as.numeric(1:9),
    two = as.numeric(11:19),
    large_int = as.numeric(9999:10007)
  )
  df[5,] = NA

  expect_type_equal(df, 'one')
  expect_type_equal(df, 'two')
  expect_type_equal(df, 'large_int')
})

test_that("empty STRING", {
  df = dplyr::tibble(A = rep("", 10))

  datafile = tempfile(fileext = '.asc')
  blafile = tempfile(fileext = '.bla')

  expect_silent(write_fwf_blaise(df, datafile, blafile, model_name = "test"))
  expect_silent({dfnew = read_fwf_blaise(datafile, blafile)})

  # I think it is reasonable to expect people to convert empty characters from
  # NA to "" themselves
  expect_equal(rep(NA_character_, 10), dfnew[['A']])
})
