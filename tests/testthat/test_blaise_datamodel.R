context("reading and parsing blaise datamodels")

makeblafile = function(model){
  blafile = tempfile('testbla', fileext = '.bla')
  writeLines(model, con = blafile)
  return(blafile)
}

test_that("correct datamodel can be read and reproduced", {
  model = "
DATAMODEL Test
FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : REAL[9,2]
  D     : STRING[4]
  E     : DATETYPE[8]  {testcomment}
{ multiline comment {with nesting}
  second line}
ENDMODEL
"
  blafile = makeblafile(model)
  expect_silent(read_model(blafile))
  bla = read_model(blafile)
  expect_true(length(bla$col_names) == 5)
  expect_true(length(bla$col_types) == 5)
  expect_true(length(bla$col_lengths) == 5)
})

test_that("Unknown datatypes throw an error", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : ONZIN1[9,2]
  D     : ONzin2[4]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error(read_model(blafile))
})

test_that("datatypes are detected", {
  model =
"
DATAMODEL Test
FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : REAL[9,2]
  D     : STRING[4]
ENDMODEL
"
  blafile = makeblafile(model)
  bla = read_model(blafile)
  expect_equal(bla$col_types[1], 'STRING')
  expect_equal(bla$col_types[2], 'INTEGER')
  expect_equal(bla$col_types[3], 'REAL')
  expect_equal(bla$col_types[4], 'STRING')
})

test_that("names are detected", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : REAL[9,2]
  D     : STRING[4]
  ENDMODEL
  "
  blafile = makeblafile(model)
  bla = read_model(blafile)
  expect_equal(bla$col_names, c('A', 'B', 'C', 'D'))
})

test_that("lengths are detected", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : REAL[9,2]
  D     : STRING[4]
  ENDMODEL
"
  blafile = makeblafile(model)
  bla = read_model(blafile)
  expect_equal(bla$col_lengths[1], 9)
  expect_equal(bla$col_lengths[2], 2)
  expect_equal(bla$col_lengths[3], 9)
  expect_equal(bla$col_lengths[4], 4)
})

test_that("floats lengths and decimals are detected", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     : STRING[9]
  B     : REAL[2]
  C     : REAL[9,2]
  D     : STRING[4]
  ENDMODEL
"
  blafile = makeblafile(model)
  bla = read_model(blafile)
  expect_equal(bla$col_lengths[2], 2)
  expect_equal(bla$col_lengths[3], 9)
  expect_true(is.na(bla$col_decimals[1]))
  expect_true(is.na(bla$col_decimals[2]))
  expect_equal(bla$col_decimals[3], 2)
})
