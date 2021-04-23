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

# http://adv-r.had.co.nz/beyond-exception-handling.html
# Hiermee kun je conditions aanmaken zodat errors hogerop afgehandeld kunnen worden
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}

#' @import stringr
# Nested comments are hard to remove with regex, therefore this helper function
remove_nested_comments = function(tekst){
  tekst <- str_replace_all(tekst, "(?s)\\\".*?\\\"", "")
  tekst0 <- ""
  while (tekst != tekst0) {
    tekst0 <- tekst
    tekst <- str_replace_all(tekst, "(?s)\\{[^\\{\\}]*\\}", "")
  }
  return(tekst)
}
