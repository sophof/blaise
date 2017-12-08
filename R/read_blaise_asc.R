#' Read a fixed width ascii datafile with a blaise datamodel
#'
#' @param datafile the ascii file with the data
#'
#' @param modelfile the datamodel describing the data
#'
#' @param dec decimal separator for real/floats, default is ","
#'
#' @export

read_blaise_asc = function(datafile, modelfile, dec = ','){
  bla = read_model(modelfile)
  data = read_data(datafile, bla, dec)
  return(data)
}
