#' Read a fixed width ascii datafile using a blaise datamodel
#'
#' @description
#' Use this function to read a fwf that is described by a blaise datamodel.
#' Only handles files with a single datamodel.
#'
#' @details
#' Currently handles the following types:
#' \itemize{
#'   \item STRING
#'   \item INTEGER
#'   \item REAL
#'   \item DATETYPE
#'   }
#'
#' Other types will throw an error unless force_string is set to TRUE, in this
#' case all columns will be read as a character vector.
#'
#' @param datafile the ascii file with the data
#'
#' @param modelfile the datamodel describing the data
#'
#' @param locale locale as specified with readr::locale(). Uses "," as default
#' decimal separator. Can be used to change date_format, timezone, encoding, etc.
#'
#' @param force_string read all unkown types as a string type. Will work as long
#' as these types are a variation of the form "unknown:unknown[2]"
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
#' "A12,3,121 1  1,00
#' B23,41,2210 20,20
#' C34,512,120100,00"
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
                           locale = readr::locale(decimal_mark = ','),
                           force_string = FALSE){
  bla = read_model(modelfile, force_string)
  data = read_data(datafile, bla, locale)
  return(data)
}
