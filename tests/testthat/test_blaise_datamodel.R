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
  F     : (Male, Female)
  G     : 1..20
  H     : 1.0..99.9
{ multiline comment {with nesting}
  second line}
ENDMODEL
"
  Ncols = 8

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(bla$col_names) == Ncols)
  expect_true(length(bla$col_types) == Ncols)
  expect_true(length(bla$col_lengths) == Ncols)
  expect_true(length(bla$col_lengths) == Ncols)
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
  E     : DATETYPE[2]
  F     : (MALE, FEMALE)
ENDMODEL
"
  blafile = makeblafile(model)
  bla = read_model(blafile)
  expect_equal(bla$col_types[1], 'STRING')
  expect_equal(bla$col_types[2], 'INTEGER')
  expect_equal(bla$col_types[3], 'REAL')
  expect_equal(bla$col_types[4], 'STRING')
  expect_equal(bla$col_types[5], 'DATETYPE')
  expect_equal(bla$col_types[6], 'ENUM')
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

test_that("ENUMS get correctly read", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     : (Male, Female)
  B     : (1,2,3,4,5,6,7,8,9,10)
  C     : INTEGER[3]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equal(bla$col_names, c('A', 'B', 'C'))
  expect_equal(bla$col_types, c('ENUM', 'ENUM', 'INTEGER'))
  expect_equal(bla$col_lengths, c(1, 2, 3))
  expect_equal(bla$col_levels[[1]],
               c('Male', 'Female'))
  expect_equal(bla$col_levels[[2]],
               c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'))
  expect_equal(bla$col_levels[[3]],
               NULL)
})

test_that("field descriptions over multiple lines work", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     :
          STRING[9]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equal(bla$col_names, 'A')
  expect_equal(bla$col_types, 'STRING')
  expect_equal(bla$col_lengths, 9)

  model =
    "
  DATAMODEL Test
  FIELDS
  A     : (Male,
           Female)
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equal(bla$col_names, 'A')
  expect_equal(bla$col_types, 'ENUM')
  expect_equal(bla$col_lengths, 1)
})


test_that("malformed datamodel throws an error", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     :  STRING[9]
  END
  "
  blafile = makeblafile(model)
  expect_error({bla = read_model(blafile)})

  model =
    "
  FIELDS
  A     :  STRING[9]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error({bla = read_model(blafile)})

  model =
    "
  DATAMODEL test
  A     :  STRING[9]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error({bla = read_model(blafile)})

  model =
    "
  DATAMODEL test
  FIELDS
  A     :  STRING
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error({bla = read_model(blafile)})

  model =
    "
  DATAMODEL test
  FIELDS
  A     :
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error({bla = read_model(blafile)})
})

