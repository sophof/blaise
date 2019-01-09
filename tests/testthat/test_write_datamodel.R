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
  expect_equal(model_names(m1), model_names(m2))
  expect_equal(model_types(m1), model_types(m2))
  expect_equal(model_widths(m1), model_widths(m2))
  expect_equal(model_decimals(m1), model_decimals(m2))
  expect_equal(model_labels(m1), model_labels(m2))
})

test_that("boolean is converted to INTEGER", {
  df = data.frame(
    bool = sample(c(T,F), 10, replace = TRUE)
  )
  df[5,] = NA

  datafile = tempfile(fileext = '.bla')
  dir = tempdir()

  expect_silent(write_datamodel(get_model(df), datafile))
  file = readr::read_file(datafile)
  model ='
  DATAMODEL
  FIELDS
  bool     : INTEGER[1]
  ENDMODEL
  '
  blafile = makeblafile(model)
  m1 = read_model(datafile)
  m2 = read_model(blafile)
  expect_equal(model_names(m1), model_names(m2))
  expect_equal(model_types(m1), model_types(m2))
  expect_equal(model_widths(m1), model_widths(m2))
})

test_that("Name can be given to datamodel", {
  df = data.frame(
    A = rep(1L,10)
  )

  datafile = tempfile(fileext = '.bla')
  dir = tempdir()

  expect_silent(write_datamodel(get_model(df), datafile, name = 'test'))
  file = readr::read_file(datafile)
  model ='
  DATAMODEL test
  FIELDS
  A     : INTEGER[1]
  ENDMODEL
  '
  blafile = makeblafile(model)
  source = read_model(datafile)
  test = read_model(blafile)
  expect_equal(name(source), name(test))
})

test_that("small REALs are accepted but padded", {
  df = data.frame(
    A = as.numeric(1:9),
    B = as.numeric(11:19),
    C = as.numeric(101:109)
  )

  datafile = tempfile(fileext = '.bla')
  dir = tempdir()

  expect_silent(write_datamodel(get_model(df), datafile, name = 'test'))
  file = readr::read_file(datafile)
  model ='
  DATAMODEL test
  FIELDS
  A     : REAL[3]
  B     : REAL[3]
  C     : REAL[3]
  ENDMODEL
  '
  blafile = makeblafile(model)
  m1 = read_model(datafile)
  m2 = read_model(blafile)
  expect_equal(model_types(m1), model_types(m2))
  expect_equal(model_widths(m1), model_widths(m2))
})
