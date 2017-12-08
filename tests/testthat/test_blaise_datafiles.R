context("reading and parsing blaise datafiles with a datamodel")

makeblafile = function(model){
  blafile = tempfile('testbla', fileext = '.bla')
  writeLines(model, con = blafile)
  return(blafile)
}

makedatafile = function(data){
  datafile = tempfile('testdata', fileext = '.asc')
  writeLines(data, con = datafile)
  return(datafile)
}

test_that("correct data and datamodel can be read and reproduced", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[1]
  B     : INTEGER[1]
  C     : REAL[3,2]
  ENDMODEL
  "
  blafile = makeblafile(model)

  data =
"A12,3
B23,4
C34,5"
  datafile = makedatafile(data)

  expect_silent(read_blaise_asc(datafile, blafile))
  df = read_blaise_asc(datafile, blafile)

  expect_identical(colnames(df), c('A', 'B', 'C'))
  expect_identical(df[[1]], c('A', 'B', 'C'))
  expect_equal(df[[2]], c(1, 2, 3))
  expect_equal(df[[3]], c(2.3, 3.4, 4.5))
})
