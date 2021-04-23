#' @import stringr
#' @import dplyr
clean_model = function(tekst){
  # Verwijder genest commentaar.
  tekst <- remove_nested_comments(tekst)

  regels <-                                        # Splits en verwijder ...
    detect_lines(tekst) %>%                  # Deel op in velden
    str_replace_all("\\r","") %>%            # Return characters
    str_replace_all("\\n","") %>%            # Newline characters
    str_replace("^[:blank:]*", "") %>%       # Spaties aan het begin;
    str_replace("[:blank:]*$", "") %>%       # Spaties aan het einde;
    str_replace_all("[:blank:]+", " ") %>%   # Spaties achter elkaar;
    str_replace_all("\".*\"", "") %>%        # Tekst tussen '"';
    str_replace_all(" *: *", ":") %>%        # Spaties rond ':';
    str_replace('\\[(\\d+)\\s*(,)?\\s*(\\d+)?\\]',
                '\\[\\1\\2\\3\\]') %>%       # Spaties tussen '[]';
    str_replace_all(" *\\[ *", "[") %>%      # Spaties rond '[';
    str_replace_all(" *\\] *", "]") %>%      # Spaties rond ']';
    str_replace_all(" *\\.\\. *", "..")      # Spaties rond '..'.
  regels <- Filter(function(x) x != "", regels) # Lege regels.


  check_bla(regels)
  return(regels)
}

detect_lines = function(text){
  options = c(
    'DATAMODEL.*',
    'FIELDS',
    'ENDMODEL',
    '.+:[\\s\\n]*[^\\(][\\d.,]+',                                                   # integer and reals repr. as 1..9
    '.+[\\s\\n]*?(\\".+\\")?[\\s\\n]*?:[\\s\\n]*[^\\(]\\w+(\\s*\\[[\\w,\\s]+\\])?', # Standard fields like STRING[1]
    'DUMMY\\s*(?:\\[\\d+\\])',                                                      # DUMMY vars
    '.+:[\\s\\n]*\\([\\s\\n,\\w\\-(\\(\\s*\\d+\\s*\\))]+\\)'                        # enums
  )
  reg = stringr::regex(
    paste0("(?:", paste(options, collapse = "|"), ")"),
    ignore_case = TRUE
  )

  unlist(stringr::str_extract_all(text, reg))
}

check_bla = function(bla){
  if (!str_detect(bla[1], regex('DATAMODEL', ignore_case = TRUE))){
    stop('Datamodel does not start with DATAMODEL but with:', bla[1])
  }
  if (!str_detect(utils::tail(bla, 1), regex('ENDMODEL', ignore_case = TRUE))){
    stop('Datamodel does not end with ENDMODEL but with: ', utils::tail(bla, 1))
  }
  if (!str_detect(bla[2], regex('FIELDS', ignore_case = TRUE))){
    stop('Datamodel does not contain FIELDS, instead found:', bla[2])
  }
}
