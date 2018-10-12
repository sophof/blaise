#' Read a fixed width ascii datafile using a blaise datamodel
#'
#' @description
#' Use this function to read a fwf that is described by a blaise datamodel.
#' Only handles files with a single datamodel.
#' If this function throws a warning, try using readr::problems() on the result,
#' this will usually show an error in the used locale.
#'
#' @details
#' Handles the following types:
#' \itemize{
#'   \item STRING
#'   \item INTEGER
#'   \item REAL
#'   \item DATETYPE
#'   \item ENUM (if numbered it will be converted to a factor with the numbers as labels)
#'   \item custom types (same as a numbered ENUM)
#'   }
#'
#' If you want the numbered enums to be converted to their labels, this is possible
#' by changing the "numbered_enum" parameter
#'
#' @param datafile the ascii file with the data
#'
#' @param modelfile the datamodel describing the data
#'
#' @param locale locale as specified with readr::locale(). Uses "." as default
#' decimal separator. Can be used to change date_format, timezone, encoding, etc.
#'
#' @param numbered_enum use actual labels instead of numbers for enums that use non-
#' standard numbering in the datamodel. With the default 'TRUE' (Male (1), Female (2), Unknown (9))
#' will be read as a factor with labels (1, 2, 9). With FALSE it will be read as a factor
#' (Male, Female, Unknown). beware that when writing a dataframe read with FALSE will result in an
#' enum with levels (1, 2, 3) since R does not support custom numbering for factors!
#'
#' @examples
#' model = "
#' DATAMODEL Test
#' FIELDS
#' A     : STRING[1]
#' B     : INTEGER[1]
#' C     : REAL[3,1]
#' D     : REAL[3]
#' E     : (Male, Female)
#' F     : 1..20
#' G     : 1.00..100.00
#' ENDMODEL
#' "
#' data =
#' "A12.3.121 1  1.00
#' B23.41.2210 20.20
#' C34.512.120100.00"
#'
#' blafile = tempfile('testbla', fileext = '.bla')
#' writeLines(model, con = blafile)
#' datafile = tempfile('testdata', fileext = '.asc')
#' writeLines(data, con = datafile)
#'
#' df = read_fwf_blaise(datafile, blafile)
#' unlink(blafile)
#' unlink(datafile)
#'
#' @export

read_fwf_blaise = function(datafile,
                           modelfile,
                           locale = readr::locale(),
                           numbered_enum = TRUE){
  bla = read_model(modelfile)
  data = read_data(datafile, bla, locale, numbered_enum)
  return(data)
}
