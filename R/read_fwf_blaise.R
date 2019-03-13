#' Read a fixed width datafile using a blaise datamodel
#'
#' @description
#' Use this function to read a fwf that is described by a blaise datamodel.
#' If this function throws a warning, try using readr::problems() on the result,
#' this will for instance show an error in the used locale.
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
#' @param datafile the fwf file containing the data
#'
#' @param modelfile the datamodel describing the data
#'
#' @param locale locale as specified with readr::locale(). Uses "." as default
#' decimal separator. Can be used to change decimal spearator, date_format, timezone, encoding, etc.
#'
#' @param numbered_enum use actual labels instead of numbers for enums that use non-
#' standard numbering in the datamodel. With the default (TRUE) (Male (1), Female (2), Unknown (9))
#' will be read as a factor with labels (1, 2, 9). With FALSE it will be read as a factor
#' (Male, Female, Unknown). beware that writing a dataframe read with FALSE will result in an
#' enum with levels (1, 2, 3) unless overruled by an existing model, since R does not support
#' custom numbering for factors.
#'
#' @param output Define which output to use. Either "data.frame" (default) or "LaF". LaF does not support
#' Datetypes, so these are converted to character vectors. Using LaF, DUMMY vasiables also can't
#' be ignored, these are read as empty character vectors. Using LaF basically takes over
#' the parsing of the datamodel from LaF, since this is more robust and accepts more types of input.
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
                           numbered_enum = TRUE,
                           output = 'data.frame'){
  bla = read_model(modelfile)
  data = switch(
    tolower(output),
    data.frame = read_data(datafile, bla, locale, numbered_enum),
    laf = read_data_laf(datafile, bla, locale, numbered_enum),
    stop(sprintf('unknown output parameter "%s"', output))
  )
  return(data)
}
