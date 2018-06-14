fill_vector = function(input_vector, replacement_vector){
  nas = is.na(input_vector)
  if(any(!nas & !is.na(replacement_vector)))
    stop("can't fill vector with replacement, non NA overlap")
  input_vector[nas] = replacement_vector[nas]
  return(input_vector)
}

#' @import stringr
#' @import dplyr
trimall = function(string){
  str_trim(string) %>%
    str_replace_all('\\n|\\r', ' ') %>%
    str_replace_all('\\s+', ' ')
}
