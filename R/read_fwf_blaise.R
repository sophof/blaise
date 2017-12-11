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
#' @param force_string read all types as a string type. Will ignore typecheck so
#' should always be successful.
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
