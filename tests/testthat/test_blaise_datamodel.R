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
  E     : DATETYPE  {testcomment}
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
  expect_true(length(variable_names(bla)) == Ncols)
  expect_true(length(variable_types(bla)) == Ncols)
  expect_true(length(variable_widths(bla)) == Ncols)
  expect_equivalent(variable_names(bla), c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'))
  expect_equivalent(variable_types(bla), c('STRING',
                                           'INTEGER',
                                           'REAL',
                                           'STRING',
                                           'DATETYPE',
                                           'ENUM',
                                           'INTEGER',
                                           'REAL'))
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
  E     : DATETYPE
  F     : (MALE, FEMALE)
ENDMODEL
"
  blafile = makeblafile(model)
  bla = read_model(blafile)
  types = variable_types(bla)
  expect_equivalent(types[1], 'STRING')
  expect_equivalent(types[2], 'INTEGER')
  expect_equivalent(types[3], 'REAL')
  expect_equivalent(types[4], 'STRING')
  expect_equivalent(types[5], 'DATETYPE')
  expect_equivalent(types[6], 'ENUM')
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
  expect_equivalent(variable_names(bla), c('A', 'B', 'C', 'D'))
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
  widths = variable_widths(bla)
  expect_equivalent(widths[1], 9)
  expect_equivalent(widths[2], 2)
  expect_equivalent(widths[3], 9)
  expect_equivalent(widths[4], 4)
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
  expect_silent({bla = read_model(blafile)})
  widths = variable_widths(bla)
  decs = variable_decimals(bla)
  expect_equivalent(widths[2], 2)
  expect_equivalent(widths[3], 9)
  expect_true(is.na(decs[1]))
  expect_true(is.na(decs[2]))
  expect_equivalent(decs[3], 2)
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
  expect_equivalent(variable_names(bla), c('A', 'B', 'C'))
  expect_equivalent(variable_types(bla), c('ENUM', 'ENUM', 'INTEGER'))
  expect_equivalent(variable_widths(bla), c(1, 2, 3))
  expect_equivalent(variable_labels(bla)[[1]],
               c('Male', 'Female'))
  expect_equivalent(variable_labels(bla)[[2]],
               c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'))
  expect_equivalent(variable_labels(bla)[[3]],
               NA_character_)
})

test_that("alternative representations for INTEGER and REAL work", {
  model = "
DATAMODEL Test
FIELDS
  G     : 1..20
  H     : 1.0..99.9
ENDMODEL
"
  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equivalent(variable_types(bla), c('INTEGER', 'REAL'))
})

test_that("Only 8 width or empty datetypes work", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : DATETYPE
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_silent(read_model(blafile))

  model = "
  DATAMODEL Test
  FIELDS
  A     : DATETYPE[8]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_silent(read_model(blafile))

  model = "
  DATAMODEL Test
  FIELDS
  A     : DATETYPE[10]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error(read_model(blafile))
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
  expect_equivalent(variable_names(bla), 'A')
  expect_equivalent(variable_types(bla), 'STRING')
  expect_equivalent(variable_widths(bla), 9)

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
  expect_equivalent(variable_names(bla), 'A')
  expect_equivalent(variable_types(bla), 'ENUM')
  expect_equivalent(variable_widths(bla), 1)
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

test_that("Nonsense decimals don't work", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     : REAL[3,3]
  ENDMODEL
"
  blafile = makeblafile(model)
  expect_error(read_model(blafile))

  model =
    "
  DATAMODEL Test
  FIELDS
  A     : REAL[3,2]
  ENDMODEL
"
  blafile = makeblafile(model)
  expect_error(read_model(blafile))
})

test_that("lowercase variables also work", {
  model = "
DATAMODEL Test
  FIELDS
  A     : String[9]
  B     : integer[2]
  C     : real[9,2]
  D     : STRING[4]
  E     : Datetype
  F     : (Male, Female)
  G     : 1..20
  H     : 1.0..99.9
  ENDMODEL
  "
  Ncols = 8

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(variable_names(bla)) == Ncols)
  expect_true(length(variable_types(bla)) == Ncols)
  expect_true(length(variable_widths(bla)) == Ncols)
  expect_equivalent(variable_names(bla), c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'))
  expect_equivalent(variable_types(bla), c('STRING',
                                           'INTEGER',
                                           'REAL',
                                           'STRING',
                                           'DATETYPE',
                                           'ENUM',
                                           'INTEGER',
                                           'REAL'))
})

test_that("DUMMY variables are accepted", {
  model = "
  DATAMODEL Test
  FIELDS
  B     : integer[2]
  DUMMY[1]
  D     : STRING[4]
  ENDMODEL
  "
  Ncols = 3

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(variable_names(bla)) == Ncols)
  expect_true(length(variable_types(bla)) == Ncols)
  expect_true(length(variable_widths(bla)) == Ncols)
  expect_equivalent(variable_names(bla), c('B', NA, 'D'))
  expect_equivalent(variable_types(bla), c('INTEGER',
                                           'DUMMY',
                                           'STRING'))
})

test_that("multiple DUMMY variables are accepted", {
  model = "
  DATAMODEL Test
  FIELDS
  B     : integer[2]
  DUMMY[1]
  DUMMY[1]
  D     : STRING[4]
  ENDMODEL
  "
  Ncols = 4

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(variable_names(bla)) == Ncols)
  expect_true(length(variable_types(bla)) == Ncols)
  expect_true(length(variable_widths(bla)) == Ncols)
  expect_equivalent(variable_names(bla), c('B', NA, NA, 'D'))
  expect_equivalent(variable_types(bla), c('INTEGER',
                                           'DUMMY',
                                           'DUMMY',
                                           'STRING'))
})

test_that("real with spaces is read", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[9]
  C     : REAL[9, 2]
  ENDMODEL
  "
  Ncols = 2

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(variable_names(bla)) == Ncols)
  expect_true(length(variable_types(bla)) == Ncols)
  expect_true(length(variable_widths(bla)) == Ncols)
  expect_equivalent(variable_names(bla), c('A', 'C'))
  expect_equivalent(variable_types(bla), c('STRING',
                                           'REAL'))
  expect_equivalent(variable_decimals(bla), c(NA, 2))
})

test_that("numbered enums work", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : (Male (1), Female (2), Unknown (9))
  B     : (M(1),F(2),X(10))
  ENDMODEL
  "
  Ncols = 2

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equivalent(variable_names(bla), c('A', 'B'))
  expect_equivalent(variable_types(bla), c('ENUM',
                                           'ENUM'))
  expect_equivalent(variable_widths(bla), c(1, 2))
})
