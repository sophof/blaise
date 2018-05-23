context("writing blaise datamodels")

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

test_that("all types are accepted", {
  df = data.frame(
    int = 1:10,
    factor = as.factor(rep(c('male','female'), 5)),
    numeric = seq(1.1111111111, 10.1111111111, by = 1),
    date = seq(as.Date('2010-01-01'), as.Date('2010-01-10'), by = 'day')
  )
  df$string = as.character(df$factor)

  datafile = tempfile(fileext = '.bla')
  dir = tempdir()

  expect_silent(write_datamodel(get_model(df), datafile))
  file = readr::read_file(datafile)
  model ='
  DATAMODEL Test
  FIELDS
  int     : INTEGER[2]
  factor  : (male, female)
  numeric : REAL[8]
  date    : DATETYPE[12]
  string  : STRING[6]
  ENDMODEL
  '
  blafile = makeblafile(model)
  expect_equal(read_model(datafile), read_model(blafile))
})

test_that("boolean is converted", {
  expect_silent(stop('to be implemented'))
})
