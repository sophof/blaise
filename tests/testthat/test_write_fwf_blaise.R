context("writing blaise datafiles without an input datamodel")

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

test_that("asc is added as extension when missing", {
  datafilename = tempfile(fileext = '.asc')
  dir = tempdir()
  df = data.frame(1:10)

  write_fwf_blaise(df, datafilename)
  files = tools::list_files_with_exts(dir, 'asc', full.names = F)
  expect_match(files, basename(datafilename), all = FALSE)
  datafilename = tempfile()
  write_fwf_blaise(df, datafilename)
  files = tools::list_files_with_exts(dir, 'asc', full.names = F)
  expect_match(files, basename(datafilename), all = FALSE)
  datafilename = tempfile(fileext = '.test')
  write_fwf_blaise(df, datafilename)
  files = tools::list_files_with_exts(dir, 'test', full.names = F)
  expect_match(files, basename(datafilename), all = FALSE)
})

test_that("bla is made automatically", {
  datafilename = tempfile(fileext = '.asc')
  blaname = stringr::str_replace(basename(datafilename), '\\.asc$', '.bla')
  dir = tempdir()
  df = data.frame(1:10)

  write_fwf_blaise(df, datafilename)
  files = tools::list_files_with_exts(dir, 'bla', full.names = F)
  expect_match(files, blaname, all = FALSE)
})

test_that("custom bla can be made", {
  datafilename = tempfile(fileext = '.asc')
  blafilename = tempfile(fileext = '.bla')
  blafilename2 = tempfile(fileext = '.test')
  dir = tempdir()
  df = data.frame(1:10)

  write_fwf_blaise(df, datafilename, blafilename)
  files = tools::list_files_with_exts(dir, 'bla', full.names = F)
  expect_match(files, basename(blafilename), all = FALSE)

  write_fwf_blaise(df, datafilename, blafilename2)
  files = tools::list_files_with_exts(dir, 'test', full.names = F)
  expect_match(files, basename(blafilename2), all = FALSE)
})

test_that("writing of bla can be prevented", {
  datafilename = tempfile(fileext = '.asc')
  blafilename = tempfile(fileext = '.bla')
  dir = tempdir()
  df = data.frame(1:10)

  write_fwf_blaise(df, datafilename, blafilename, write_model = FALSE)
  files = tools::list_files_with_exts(dir, 'bla', full.names = F)
  expect_false(any(stringr::str_detect(files, basename(blafilename))))
})

test_that("data.frame can be written", {
  datafilename = tempfile(fileext = '.asc')
  blafilename = tempfile(fileext = '.bla')
  df = data.frame(1:10)

  expect_silent(write_fwf_blaise(df, datafilename, blafilename))
})

test_that("tibble can be written", {
  datafilename = tempfile(fileext = '.asc')
  blafilename = tempfile(fileext = '.bla')
  df = dplyr::tibble(1:10)

  expect_silent(write_fwf_blaise(df, datafilename, blafilename))
})

test_that("output is returned as character vector", {
  datafilename = tempfile(fileext = '.asc')
  blafilename = tempfile(fileext = '.bla')
  df = dplyr::tibble(9:11)

  expect_silent({res = write_fwf_blaise(df, datafilename, blafilename)})
  expect_equal(res, c(" 9", "10", "11"))
})

test_that("padding direction can be supplied", {
  dir = tempdir()
  datafile = tempfile('testasc', dir, fileext = '.asc')

  df = data.frame(
    list(
      A = c('t', 'te', 'tes'),
      B = c(1, 22, 333)
    ),
    stringsAsFactors = FALSE
  )

  expect_silent(write_fwf_blaise(df, datafile, justify = 'left'))
  uit = readr::read_file(datafile)
  expect_equal(uit, c('t    1\r\nte  22\r\ntes333\r\n'))
  expect_silent(write_fwf_blaise(df, datafile, justify = 'right'))
  uit = readr::read_file(datafile)
  expect_equal(uit, c('  t  1\r\n te 22\r\ntes333\r\n'))
  expect_silent(write_fwf_blaise(df, datafile, justify = 'centre'))
  uit = readr::read_file(datafile)
  expect_equal(uit, c(' t   1\r\nte  22\r\ntes333\r\n'))
})

test_that("Automatic name to datamodel", {
  df = data.frame(
    A = rep(1L,10)
  )

  datafile = tempfile(fileext = '.asc')
  blafile = tempfile(fileext = '.asc')

  expect_silent(write_fwf_blaise(df, datafile, blafile))
  model ='
  DATAMODEL df
  FIELDS
  A     : INTEGER[1]
  ENDMODEL
  '
  testblafile = makeblafile(model)
  source = read_model(blafile)
  test = read_model(testblafile)
  expect_equal(name(source), name(test))
})

test_that("custom name to datamodel", {
  df = data.frame(
    A = rep(1L,10)
  )

  datafile = tempfile(fileext = '.asc')
  blafile = tempfile(fileext = '.asc')

  expect_silent(write_fwf_blaise(df, datafile, blafile, model_name = 'test'))
  model ='
  DATAMODEL test
  FIELDS
  A     : INTEGER[1]
  ENDMODEL
  '
  testblafile = makeblafile(model)
  source = read_model(blafile)
  test = read_model(testblafile)
  expect_equal(name(source), name(test))
})

test_that("dataframe can be written with only one row", {
  df = data.frame(
    A = 'A',
    B = 1L,
    C = 2.50
  )

  datafile = tempfile(fileext = '.asc')
  blafile = tempfile(fileext = '.asc')

  expect_silent(write_fwf_blaise(df, datafile, blafile, model_name = 'test'))
})

test_that("dataframe can be written with small reals", {
  df = data.frame(
    A = as.numeric(1:9),
    B = as.numeric(11:19),
    C = as.numeric(99:107)
  )

  datafile = tempfile(fileext = '.asc')
  blafile = tempfile(fileext = '.asc')

  expect_silent(write_fwf_blaise(df, datafile, blafile, model_name = 'test'))
})
