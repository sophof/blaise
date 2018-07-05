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

test_that("converted dataframe is returned", {
  datafilename = tempfile(fileext = '.asc')
  blafilename = tempfile(fileext = '.bla')
  df = dplyr::tibble(9:11)

  expect_silent({res = write_fwf_blaise(df, datafilename, blafilename)})
  expect_equal(res, df)
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
  expect_equal(uit, c('t    1\nte  22\ntes333\n'))
  expect_silent(write_fwf_blaise(df, datafile, justify = 'right'))
  uit = readr::read_file(datafile)
  expect_equal(uit, c('  t  1\n te 22\ntes333\n'))
  expect_silent(write_fwf_blaise(df, datafile, justify = 'centre'))
  uit = readr::read_file(datafile)
  expect_equal(uit, c(' t   1\nte  22\ntes333\n'))
})
