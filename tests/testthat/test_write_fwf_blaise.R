context("writing blaise datafiles with a datamodel")

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

test_that("data.frame can be written", {
  datafilename = tempfile(fileext = '.asc')
  blafilename = tempfile(fileext = '.bla')
  blafilename2 = tempfile(fileext = '.test')
  dir = tempdir()
  df = data.frame(1:10)

  expect_silent(write_fwf_blaise(df, datafilename, blafilename))
})

test_that("tibble can be written", {
  datafilename = tempfile(fileext = '.asc')
  blafilename = tempfile(fileext = '.bla')
  blafilename2 = tempfile(fileext = '.test')
  dir = tempdir()
  df = dplyr::tibble(1:10)

  expect_silent(write_fwf_blaise(df, datafilename, blafilename))
})
