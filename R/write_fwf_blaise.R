#' Write a fixed width ascii datafile and accompanying blaise datamodel
#'
#' @description Write a datafile in the blaise format (fwf ascii without separators)
#' will always write out a blaise datamodel describing the datafile as well
#'
#' @details Currently supports the following dataformats:
#' \itemize{
#'   \item character = STRING,
#'   \item integer = INTEGER,
#'   \item numeric = REAL,
#'   \item Date = DATETYPE,
#'   \item factor = STRING
#' }
#'
#' @param df dataframe to write
#' @param output_data path and name to output datafile. Will add .asc if no extension
#' @param output_model path and name to output datamodel. If NULL will use the
#' same name as output_data with .bla extension unless and input_model is given.
#' @param force_string If TRUE, will force unknown/unsupported column types to
#' string, otherwise these will throw an error
#' @param decimal.mark decimal mark to use. Default is ",".
#' @param digits how many significant digits are to be used for numeric and
#' complex x. The default uses getOption("digits"). This is a suggestion:
#' enough decimal places will be used so that the smallest (in magnitude) number
#' has this many significant digits.
#' @param justify direction of padding for STRING type when data is smaller than the width.
#' Defaults to right-justified (padded on the left), can be "left", "right" or "centre".
#' @param write_model logical that can be used to disabling the automatic writing of a
#' datamodel
#'
#' @return dataframe written to file. Does so invisibly, will not print but can be assigned.
#'
#' @export
#'
#' @examples
#' datafilename = tempfile('testdata', fileext = '.asc')
#' blafilename = tempfile('testbla', fileext = '.bla')
#' data = data.frame(1, 1:10, sample(LETTERS[1:3], 10, replace = TRUE), runif(10, 1, 10))
#' write_fwf_blaise(data, datafilename)
#' write_fwf_blaise(data,
#'   datafilename,
#'   blafilename,
#'   force_string = TRUE)
#' unlink(c(datafilename, blafilename))
#'
write_fwf_blaise = function(df,
                     output_data,
                     output_model = NULL,
                     force_string = FALSE,
                     decimal.mark = ',',
                     digits = getOption('digits'),
                     justify = 'left',
                     write_model = TRUE){
  # add asc if no file extension found
  if (tools::file_ext(output_data) == ''){
    output_data = paste0(output_data, '.asc')
  }
  # create output_model filename if not given
  if(is.null(output_model)){
    output_model = tools::file_path_sans_ext(output_data)
    output_model = paste0(output_model, '.bla')
  }

  model = get_model(df, digits, force_string)
  df = write_data(df, model, file = output_data, decimal.mark, justify = justify)
  if(write_model) write_datamodel(model, output_model)
  return(invisible(df))
}
