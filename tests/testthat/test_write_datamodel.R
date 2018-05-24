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
  DATAMODEL
  FIELDS
  int     : INTEGER[2]
  factor  : (female, male)
  numeric : REAL[9]
  date    : DATETYPE[8]
  string  : STRING[6]
  ENDMODEL
  '
  blafile = makeblafile(model)
  m1 = read_model(datafile)
  m2 = read_model(blafile)
  expect_equal(variable_names(m1), variable_names(m2))
  expect_equal(variable_types(m1), variable_types(m2))
  expect_equal(variable_widths(m1), variable_widths(m2))
  expect_equal(variable_decimals(m1), variable_decimals(m2))
  expect_equal(variable_labels(m1), variable_labels(m2))
})

test_that("boolean is converted", {
  expect_silent(stop('to be implemented'))
})
