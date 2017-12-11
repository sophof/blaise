#' Write a fixed width ascii datafile and accompanying blaise datamodel
#'
#' @description Write a datafile in the blaise format (fwf ascii without separators)
#' will always write out a blaise datamodel describing the datafile as well
#'
#' @param df dataframe to write
#' @param output_data path and name to output datafile. Will add .asc if no extension
#' @param output_model path and name to output datamodel. If NULL will use the
#' same name as output_data with .bla extension.
#' @param force_string If TRUE, will force unknown/unsupported column types to
#' string, otherwise these will throw an error
#' @param ... Extra options to be passed to format(). Can be used to adjust defaults
#' such as significant digits for doubles and maximum field width. Use ?format()
#' for more details. uses decimal.mark "," by default, all other defaults stay the
#' same.
#'
#' @return
#' @export
#'
#' @examples
#' datafilename = tempfile('testdata', fileext = '.asc')
#' blafilename = tempfile('testbla', fileext = '.bla')
#' data = data.frame(1, 1:10, sample(LETTERS[1:3], 10, replace = TRUE), runif(10, 1, 10))
#' # would automatically create a bla file, but fails because factor is an
#' # unsupported type
#' write_fwf_blaise(data, datafilename)
#' # Will convert factor to string type
#' write_fwf_blaise(data,
#'   datafilename,
#'   blafilename,
#'   force_string = TRUE)
#'
write_fwf_blaise = function(df,
                            output_data,
                            output_model = NULL,
                            force_string = FALSE,
                            ...){
  # add asc if no file extension found
  if (tools::file_ext(output_data) == ''){
    output_data = paste0(output_data, '.asc')
  }

  # create output_model filename if not given
  if(is.null(output_model)){
    output_model = tools::file_path_sans_ext(output_data)
    output_model = paste0(output_model, '.bla')
  }

  df = convert_df(df, ...) # Converts columns to single character vector

  write_asc(df, output_data, locale)
}
