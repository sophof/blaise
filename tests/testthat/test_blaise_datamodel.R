context("reading and parsing blaise datamodels")

bla = "
DATAMODEL Test
FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : REAL[9,2]
  D     : STRING[4]
  E     : STRING[2]  {testcomment}
  F     : ONZIN[5] {moet vervallen}
ENDMODEL
"
blafile = tempfile('testbla', fileext = '.bla')
writeLines(bla, con = blafile)

test_that("datamodel can be read and reproduced", {
  expect_silent(read_model(blafile))
  bla = read_model(blafile)
  expect_true(length(bla$col_names) == 5)
  expect_true(length(bla$col_types) == 5)
  expect_true(length(bla$col_lengths) == 5)
})
