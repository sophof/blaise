#' Write a fixed width ascii datafile based on a given blaise datamodel
#'
#' @description Write a datafile in the blaise format (fwf ascii without separators)
#' using an existing datamodel. will not write out a datamodel unless explicitly asked to.
#' Tries to automatically match colummns by name using Levenshtein distance and will
#' change types if required
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
#' @param input_model the datamodel used to convert the dataframe and write the output
#' @param output_data path and name to output datafile. Will add .asc if no extension
#' @param output_model path and name to output datamodel. If NULL will not write anything.
#' default is NULL
#' @param decimal.mark decimal mark to use. Default is ",".
#' @param justify direction of padding for STRING type when data is smaller than the width.
#' Defaults to right-justified (padded on the left), can be "left", "right" or "centre".
#' @param max.distance maximum Levenshtein distance to match columns. ignores case changes.
#' Set to 0 (default) to only accept exact matches ignoring case. 4 appears to be
#' a good number in general. Will prevent double matches and will pick te best match
#' for each variable in the datamodel.
#'
#' @return dataframe written
#' @export
#'
#' @examples
#'
write_fwf_with_model = function(df,
                                output_data,
                                input_model,
                                output_model = NULL,
                                decimal.mark = ',',
                                digits = getOption('digits'),
                                justify = 'left',
                                max.distance = 0L){
  # add asc if no file extension found
  if (tools::file_ext(output_data) == ''){
    output_data = paste0(output_data, '.asc')
  }

  model = read_model(input_model, FALSE)
  df = convert_df(df, model, max.distance = max.distance)
  df = write_data(df, model, file = output_data, decimal.mark, justify = justify)
  if(!is.null(output_model)) write_datamodel(model, output_model)
  return(invisible(df))
}
