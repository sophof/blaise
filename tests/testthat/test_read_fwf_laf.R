context("reading and parsing blaise datafiles with a datamodel outputting a LaF object")

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

test_that("correct data and generic datamodel can be read and reproduced", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[1]
  B     : INTEGER[1]
  C     : REAL[3,1]
  D     : REAL[3]
  E     : (Male, Female)
  F     : 1..20
  G     : 1.00..100.00
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "A12.3.121 1  1.00\nB23.41.2210 20.20\nC34.512.120100.00"
  datafile = makedatafile(data)

  expect_silent({df = read_fwf_blaise(datafile, blafile, output = "laf")})
  expect_equal(class(df)[1], "laf")
  df = df[,]
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

  data = "123\n23,\n3,4"
  datafile = makedatafile(data)

  expect_silent({df = read_fwf_blaise(datafile,
                                      blafile,
                                      locale = readr::locale(decimal_mark =  ','),
                                      output = "laf")})
  expect_equal(df[[1]][], c(123, 23., 3.4))
  unlink(blafile)
  unlink(datafile)
})

test_that("DATETYPE can be used, will be read as character", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : DATETYPE[8]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "20100401\n20110502\n20120603"
  datafile = makedatafile(data)

  df = read_fwf_blaise(datafile,
                       blafile,
                       locale = readr::locale(date_format = '%Y%m%d'),
                       output = 'laf')
  expect_equal(df[[1]][], c("20100401", "20110502", "20120603"))
  unlink(blafile)
  unlink(datafile)
})

test_that("empty values are read as NA, this does not work for strings", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[1]
  B     : INTEGER[1]
  C     : (Man, Vrouw)
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "A1 \n 22\nC 1"
  datafile = makedatafile(data)

  expect_silent({df = read_fwf_blaise(datafile, blafile, output = "laf")[,]})
  expect_equal(df[[1]], c('A', '', 'C'))
  expect_equal(df[[2]], c(1, 2, NA))
  expect_equal(df[[3]], factor(c(NA, 'Vrouw', 'Man'), levels = c('Man', 'Vrouw')))
})

test_that("integers outside of max.integer range produce a warning and are converted to double", {
  model = "
  DATAMODEL Test
  FIELDS
  B     : INTEGER[10]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "         1\n9999999999\n       -30"
  datafile = makedatafile(data)

  expect_warning({df = read_fwf_blaise(datafile, blafile, output = "laf")[,]})
  expect_equal(df[[1]], c(1, 9999999999, -30))
  expect_equal(class(df[[1]]), 'numeric')
})

test_that("floats of max.range produce a warning and are converted to string", {
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

  expect_warning({df = read_fwf_blaise(datafile, blafile, output = "laf")[,]})
  expect_equal(df[[1]], c('1', '2', c))
  expect_equal(class(df[[1]]), 'character')
})

test_that("DUMMY variables are read as empty character vector", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[1]
  DUMMY[1]
  B     : INTEGER[1]
  C     : (M, F)
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "A 11\nB 22\nC 31"
  datafile = makedatafile(data)

  expect_silent({df = read_fwf_blaise(datafile, blafile, output = "laf")[,]})
  expect_equal(colnames(df), c('A', 'DUMMY1', 'B', 'C'))
  expect_equal(ncol(df), 4)
  expect_equal(df[[2]], character(3))
})
