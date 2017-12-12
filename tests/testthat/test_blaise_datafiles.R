context("reading and parsing blaise datafiles with a datamodel")

makeblafile = function(model){
  blafile = tempfile('testbla', fileext = '.bla')
  writeLines(model, con = blafile)
  return(blafile)
}

makedatafile = function(data){
  datafile = tempfile('testdata', fileext = '.asc')
  writeLines(data, con = datafile)
  return(datafile)
}

test_that("correct data and datamodel can be read and reproduced", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[1]
  B     : INTEGER[1]
  C     : REAL[3,2]
  D     : REAL[3]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data =
"A12,3,12
B23,41,2
C34,512,"
  datafile = makedatafile(data)

  expect_silent({df = read_fwf_blaise(datafile, blafile)})
  expect_identical(colnames(df), c('A', 'B', 'C', 'D'))
  expect_identical(df[[1]], c('A', 'B', 'C'))
  expect_equal(df[[2]], c(1, 2, 3))
  expect_equal(df[[3]], c(2.3, 3.4, 4.5))
  expect_equal(df[[4]], c(.12, 1.2, 12.))
  unlink(blafile)
  unlink(datafile)
})


test_that("different decimal separator can be used", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : REAL[3]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data =
"123
23.
3.4"
  datafile = makedatafile(data)

  df = read_fwf_blaise(datafile,
                       blafile,
                       locale = readr::locale(decimal_mark =  '.'))
  expect_equal(df[[1]], c(123, 23., 3.4))
  unlink(blafile)
  unlink(datafile)
})

test_that("DATETYPE can be used", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : DATETYPE[8]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data =
"20100401
20110502
20120603"
  datafile = makedatafile(data)

  df = read_fwf_blaise(datafile,
                       blafile,
                       locale = readr::locale(date_format = '%Y%m%d'))
  expect_equal(lubridate::day(df[[1]]), c(1, 2, 3))
  expect_equal(lubridate::month(df[[1]]), c(4, 5, 6))
  expect_equal(lubridate::year(df[[1]]), c(2010, 2011, 2012))
  unlink(blafile)
  unlink(datafile)

  model = "
  DATAMODEL Test
  FIELDS
  A     : DATETYPE[10]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data =
"2010-04-01
2011-05-02
2012-06-03"
  datafile = makedatafile(data)

  df = read_fwf_blaise(datafile,
                       blafile)
  expect_equal(lubridate::day(df[[1]]), c(1, 2, 3))
  expect_equal(lubridate::month(df[[1]]), c(4, 5, 6))
  expect_equal(lubridate::year(df[[1]]), c(2010, 2011, 2012))
  unlink(blafile)
  unlink(datafile)
})

test_that("unknown types throw an error", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : INTEGER[1]
  B     : ONZIN[2]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "123\n23.\n3.4"
  datafile = makedatafile(data)

  expect_error(read_fwf_blaise(datafile, blafile))
  unlink(blafile)
  unlink(datafile)
})

test_that("string can be forced to read unknown types", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : INTEGER[1]
  B     : ONZIN[2]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "123\n23.\n3.4"
  datafile = makedatafile(data)

  expect_silent(read_fwf_blaise(datafile, blafile, force_string = TRUE))

  df = read_fwf_blaise(datafile, blafile, force_string = TRUE)

  expect_identical(colnames(df), c('A', 'B'))
  expect_identical(df[[1]], c('1', '2', '3'))
  expect_identical(df[[2]], c('23', '3.', '.4'))
  unlink(blafile)
  unlink(datafile)
})

test_that("dataframe is read as a tibble", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[1]
  B     : INTEGER[1]
  C     : REAL[3,2]
  D     : REAL[3]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "A12,3,12\nB23,41,2\nC34,512,"
  datafile = makedatafile(data)

  df = read_fwf_blaise(datafile, blafile)
  expect_match(class(df), '^tbl', all = FALSE)
})
