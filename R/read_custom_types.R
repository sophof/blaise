# extract types section from modelfile and parse the types. Returns a list of types
#' @import stringr
#' @import dplyr
read_custom_types = function(bla){
  block = extract_type_block(bla)
  if(is.na(block[[1]])) return(NULL)
  types = extract_custom_types(block)
  if(is.na(types[[1]])) return(NULL)
  ret = lapply(types, parse_type)
  names(ret) = sapply(ret, name)
  return(ret)
}

parse_type = function(type){
  name = str_match(type, '^(\\w+)=.*$')[,2]
  l = str_match_all(type, '(\\w+)\\((\\d+)\\)')[[1]]
  labels = l[,2]
  indices = l[,3]
  variable_custom(name, labels = labels, levels = indices)
}

extract_custom_types = function(block){
  reg = regex(
    '\\w+\\s*?=\\s*?\\(.+?\\)\\s*?\\)',
    ignore_case = TRUE,
    dotall = TRUE
  )
  str_match_all(block, reg)[[1]][,1] %>%
    str_replace_all("\\r","") %>%            # Return characters
    str_replace_all("\\n","") %>%            # Newline characters
    str_replace("^[:blank:]*", "") %>%       # Spaties aan het begin
    str_replace("[:blank:]*$", "") %>%       # Spaties aan het einde
    str_replace_all("[:blank:]+", " ") %>%   # Spaties achter elkaar
    str_replace_all("\".*\"", "") %>%        # Tekst tussen '"'
    str_replace_all('[:blank:]', '')         # Spaties in het algemeen
}

extract_type_block = function(bla){
  # Verwijder genest commentaar.
  bla <- remove_nested_comments(bla)

  #pak datamodel
  bla = str_extract(bla, regex('(?<=DATAMODEL).*(?=ENDMODEL)',
                                 ignore_case = TRUE,
                                 dotall = TRUE))

  #pak TYPE section
  reg = regex('(?<=(?<!DATE)TYPE)\\s*.*\\)\\s*\\)\\s*(?=FIELDS)',
              ignore_case = TRUE,
              dotall = TRUE)
  str_extract(bla, reg)
}
