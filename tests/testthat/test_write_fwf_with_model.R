context("converting a dataframe according to an input datamodel")

makeblafile = function(model, file = NULL){
  if (is.null(file)) file = tempfile('testbla', fileext = '.bla')
  writeLines(model, con = file)
  return(file)
}

makedatafile = function(data, file = NULL){
  if (is.null(file)) file = tempfile('testdata', fileext = '.asc')
  writeLines(data, con = file)
  return(file)
}

test_that("input_model passes an identical dataframe with common types", {
  dir = tempdir()
  datafile = tempfile('testasc', dir, fileext = '.asc')
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[1]
  B     : INTEGER[1]
  C     : REAL[3,2]
  D     : REAL[3]
  E     : (Male, Female)
  F     : 1..20
  G     : 1.00..100.00
  H     : DATETYPE[8]
  ENDMODEL
  "
  blafile = makeblafile(model)

  df = data.frame(
    list(
      A = rep('t',3),
      B = 1:3,
      C = 1.1:3.3,
      D = 1.0:3.0,
      E = factor(c(1,2,1), labels = c('M', 'F')),
      F = 1:3,
      G = c(1., 99.9, 78.5),
      H = as.Date(rep('2001-01-01', 3))
    )
  )

  expect_silent(write_fwf_with_model(df, datafile, blafile))
  unlink(c(datafile, blafile))
})

test_that("input_model pads int and string types if width is larger than column", {
  dir = tempdir()
  datafile = tempfile('testasc', dir, fileext = '.asc')
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[2]
  B     : INTEGER[2]
  C     : REAL[3]
  ENDMODEL
  "
  blafile = makeblafile(model)

  df = data.frame(
    list(
      A = rep('t',3),
      B = 1:3,
      C = 1.1:3.1
    ),
    stringsAsFactors = FALSE
  )

  expect_silent(write_fwf_with_model(df, datafile, blafile, decimal.mark = '.'))
  expect_silent(newdf <- readr::read_fwf(
    datafile,
    col_positions = readr::fwf_widths(c(2, 2, 3)),
    col_types = 'cid',
    progress = FALSE))
  expect_equivalent(ncol(newdf), 3)
  expect_equivalent(newdf[[1]], rep('t',3))
  expect_equivalent(newdf[[2]], 1:3)
  expect_equivalent(newdf[[3]], 1.1:3.1)
  unlink(c(datafile, blafile))
})

test_that("REAL type is converted to correct significance with warning", {
  dir = tempdir()
  datafile = tempfile('testasc', dir, fileext = '.asc')
  model = "
  DATAMODEL Test
  FIELDS
  C     : REAL[3,2]
  D     : REAL[3]
  G     : 1.00..99.99
  ENDMODEL
  "
  blafile = makeblafile(model)

  df = data.frame(
    list(
      C = 1.11:3.11,
      D = 1.01:3.01,
      G = c(1.111, 99.911, 78.511)
    )
  )

  expect_message(write_fwf_with_model(df, datafile, input_model = blafile, decimal.mark = '.'))
  expect_silent(newdf <- readr::read_fwf(
    datafile,
    col_positions = readr::fwf_widths(c(3, 3, 6)),
    col_types = 'ddd',
    progress = FALSE))
  expect_equivalent(ncol(newdf), 3)
  expect_equivalent(newdf[[1]], c(1.1, 2.1, 3.1))
  expect_equivalent(newdf[[2]], c(1.0, 2.0, 3.0))
  expect_equivalent(newdf[[3]], c(1.11, 99.91, 78.51))
  unlink(c(datafile, blafile))
})

test_that("larger INTS, STRINGS throw an error", {
  dir = tempdir()
  datafile = tempfile('testasc', dir, fileext = '.asc')
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[3]
  B     : INTEGER[1]
  ENDMODEL
  "
  blafile = makeblafile(model)

  df = data.frame(
    list(
      A = rep('tst',3),
      B = 11:13
    ),
    stringsAsFactors = FALSE
  )

  expect_error(write_fwf_with_model(df, datafile, input_model = blafile))
  df = data.frame(
    list(
      A = rep('test',3),
      B = 1:3
    ),
    stringsAsFactors = FALSE
  )
  expect_error(write_fwf_with_model(df, datafile, input_model = blafile))
  unlink(c(datafile, blafile))
})

test_that("unavailable/unknown variables throw an error", {
  dir = tempdir()
  datafile = tempfile('testasc', dir, fileext = '.asc')
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[3]
  B     : INTEGER[1]
  ENDMODEL
  "
  blafile = makeblafile(model)

  df = data.frame(
    list(
      A = rep('t',3)
    ),
    stringsAsFactors = FALSE
  )
  expect_error(write_fwf_with_model(df, datafile, input_model = blafile))
  unlink(c(datafile, blafile))
})

test_that("extra columns in dataframe are ignored", {
  dir = tempdir()
  datafile = tempfile('testasc', dir, fileext = '.asc')
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[3]
  ENDMODEL
  "
  blafile = makeblafile(model)

  df = data.frame(
    list(
      A = rep('tst',3)
    ),
    stringsAsFactors = FALSE
  )
  expect_silent(write_fwf_with_model(df, datafile, input_model = blafile))
  expect_silent(newdf <- readr::read_fwf(
    datafile,
    col_positions = readr::fwf_widths(c(3)),
    col_types = 'c',
    progress = FALSE))
  expect_equivalent(newdf[[1]], rep('tst',3))
  expect_equivalent(ncol(newdf), 1)
  unlink(c(datafile, blafile))
})

test_that("order in dataframe doesn't matter", {
  dir = tempdir()
  datafile = tempfile('testasc', dir, fileext = '.asc')
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[3]
  B     : INTEGER[1]
  ENDMODEL
  "
  blafile = makeblafile(model)

  df = data.frame(
    list(
      B = 1:3,
      A = rep('tst',3)
    ),
    stringsAsFactors = FALSE
  )

  expect_silent(write_fwf_with_model(df, datafile, input_model = blafile))
  expect_silent(newdf <- readr::read_fwf(
    datafile,
    col_positions = readr::fwf_widths(c(3, 1)),
    col_types = 'ci',
    progress = FALSE))
  expect_equivalent(ncol(newdf), 2)
  expect_equivalent(newdf[[1]], rep('tst',3))
  expect_equivalent(newdf[[2]], 1:3)
  unlink(c(datafile, blafile))
})

test_that("types are converted properly and can be converted back without loss", {
  expect_type_equal = function(df, dfnew, column, cast){
    datafile = tempfile(fileext = '.asc')
    df[[column]] = cast(df[[column]])
    eval(bquote(expect_equivalent(.(df)[[.(column)]], dfnew[[.(column)]], tolerance = 1e-7)))
  }

  dir = tempdir()
  datafile = tempfile('testasc', dir, fileext = '.asc')
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[1]
  B     : STRING[1]
  C     : STRING[3]
  E     : STRING[1]
  H     : STRING[8]
  ENDMODEL
  "
  blafile = makeblafile(model)

  df = data.frame(
    list(
      A = rep('t',3),
      B = 1:3,
      C = 1.1:3.3,
      E = factor(c(1,2,1), labels = c('M', 'F')),
      H = as.Date(rep('2001-01-01', 3))
    ),
    stringsAsFactors = FALSE
  )

  expect_silent(write_fwf_with_model(df, datafile, input_model = blafile))
  expect_silent(newdf <- read_fwf_blaise(
    datafile,
    blafile))
  expect_type_equal(df, dfnew, 'B', as.integer)
  expect_type_equal(df, dfnew, 'C', as.double)
  expect_type_equal(df, dfnew, 'E', as.factor)
  expect_type_equal(df, dfnew, 'H', as.Date)
  unlink(c(datafile, blafile))
})
