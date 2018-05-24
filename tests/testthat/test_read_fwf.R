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
  E     : (Male, Female)
  F     : 1..20
  G     : 1.00..100.00
  ENDMODEL
  "
  blafile = makeblafile(model)

  data =
"A12,3,121 1  1,00
B23,41,2210 20,20
C34,512,120100,00"
  datafile = makedatafile(data)

  expect_silent({df = read_fwf(datafile, blafile)})
  expect_identical(colnames(df), c('A', 'B', 'C', 'D', 'E', 'F', 'G'))
  expect_identical(df[[1]], c('A', 'B', 'C'))
  expect_equal(df[[2]], c(1, 2, 3))
  expect_equal(df[[3]], c(2.3, 3.4, 4.5))
  expect_equal(df[[4]], c(.12, 1.2, 12.))
  expect_equal(df[[5]], factor(c(1, 2, 1), labels = c('Male', 'Female')))
  expect_equal(df[[6]], c(1, 10, 20))
  expect_equal(df[[7]], c(1.00, 20.20, 100.00))
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

  expect_silent({df = read_fwf(datafile,
                       blafile,
                       locale = readr::locale(decimal_mark =  '.'))})
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

  df = read_fwf(datafile,
                       blafile,
                       locale = readr::locale(date_format = '%Y%m%d'))
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

  expect_error(read_fwf(datafile, blafile))
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

  expect_silent(read_fwf(datafile, blafile, force_string = TRUE))

  df = read_fwf(datafile, blafile, force_string = TRUE)

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

  df = read_fwf(datafile, blafile)
  expect_match(class(df), '^tbl', all = FALSE)
})

test_that("empty values are read as NA", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[1]
  B     : INTEGER[1]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "A1\n 2\nC "
  datafile = makedatafile(data)

  expect_silent({df = read_fwf(datafile, blafile)})
  expect_equal(df[[1]], c('A', NA, 'C'))
  expect_equal(df[[2]], c(1, 2, NA))
})

test_that("negative integers work", {
  model = "
  DATAMODEL Test
  FIELDS
  B     : INTEGER[3]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "  1\n999\n-30"
  datafile = makedatafile(data)

  expect_silent({df = read_fwf(datafile, blafile)})
  expect_equal(df[[1]], c(1, 999, -30))
})

test_that("integers outside of max.integer range produce a warning and are converted to double", {
  model = "
  DATAMODEL Test
  FIELDS
  B     : INTEGER[10]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "        1\n9999999999\n       -30"
  datafile = makedatafile(data)

  expect_warning({df = read_fwf(datafile, blafile)})
  expect_equal(df[[1]], c(1, 9999999999, -30))
  expect_equal(class(df[[1]]), 'numeric')
})

test_that("integers floats of max.range produce a warning and are converted to string", {
  model = "
  DATAMODEL Test
  FIELDS
  B     : REAL[54]
  ENDMODEL
  "
  blafile = makeblafile(model)

  leeg50 = strrep(' ', 50)
  a = paste0(strrep(' ', 50), '   1')
  b = paste0(strrep(' ', 50), '   2')
  c = strrep('9', 54)
  data = paste(a,b,c, sep = '\n')
  datafile = makedatafile(data)

  expect_warning({df = read_fwf(datafile, blafile)})
  expect_equal(df[[1]], c('1', '2', c))
  expect_equal(class(df[[1]]), 'character')
})