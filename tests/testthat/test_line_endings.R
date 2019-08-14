context("test line ending behaviour")

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

test_that("cr lf line endings are written by write_fwf_blaise", {
  datafilename = tempfile(fileext = '.asc')
  dir = tempdir()
  df = data.frame(A = 1:10)

  write_fwf_blaise(df, datafilename)
  file = readr::read_file(datafilename)
  expect_true(stringr::str_ends(file, "\r\n"))
})

test_that("cr lf line endings are written by write_fwf_blaise_with_model", {
  datafilename = tempfile(fileext = '.asc')
  dir = tempdir()
  df = data.frame(A = 1:10)
  model = "
  DATAMODEL Test
  FIELDS
  A     : INTEGER[2]
  ENDMODEL
  "
  blafile = makeblafile(model)

  write_fwf_blaise_with_model(df, datafilename, blafile)
  file = readr::read_file(datafilename)
  expect_true(stringr::str_ends(file, "\r\n"))
})

test_that("read_fwf_blaise can read crlf (windows) files", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : INTEGER[1]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "1\r\n2\r\n3\r\n"
  datafile = makedatafile(data)
  expect_silent({df = read_fwf_blaise(datafile, blafile)})
  expect_equal(df[[1]], c(1, 2, 3))
  unlink(blafile)
  unlink(datafile)
})

test_that("read_fwf_blaise can read lf only (unix) files", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : INTEGER[1]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data = "1\n2\n3\n"
  datafile = makedatafile(data)
  expect_silent({df = read_fwf_blaise(datafile, blafile)})
  expect_equal(df[[1]], c(1, 2, 3))
  unlink(blafile)
  unlink(datafile)
})
