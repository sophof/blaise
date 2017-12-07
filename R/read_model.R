read_model = function(blafile){
  bla = readr::read_file(blafile)
  bla = parse_bla(bla)
  return(bla)
}

parse_bla = function(bla){
  bla = clean_model(bla)
  modelname = extract_datamodelName(bla)
  names = extract_names(bla)
  types = extract_types(bla)
  lengths = extract_lengths(bla)
  return(list(modelname = modelname,
              col_names = names,
              col_types = types,
              col_lengths = lengths))
}

#' @import stringr
#' @import dplyr
clean_model = function(tekst){
  # Verwijder genest commentaar.
  tekst0 <- ""
  while (tekst != tekst0) {
    tekst0 <- tekst
    tekst <- str_replace_all(tekst, "(?s)\\{[^\\{\\}]*\\}", "")
  }

  regels <-                                        # Splits en verwijder ...
    unlist(str_split(tekst, "\n")) %>%
    str_replace_all("\\r","") %>%            # Return characters
    str_replace("^[:blank:]*", "") %>%       # Spaties aan het begin;
    str_replace("[:blank:]*$", "") %>%       # Spaties aan het einde;
    str_replace_all("[:blank:]+", " ") %>%   # Spaties achter elkaar;
    str_replace_all("\".*\"", "") %>%        # Tekst tussen '"';
    str_replace_all(" *: *", ":") %>%        # Spaties rond ':';
    str_replace_all(" *\\[ *", "[") %>%      # Spaties rond '[';
    str_replace_all(" *\\] *", "]") %>%      # Spaties rond ']';
    str_replace_all(" *\\.\\. *", "..") %>%  # Spaties rond '..'.
    Filter(function(x) x != "", .)           # Lege regels.
  return(regels)
}

extract_datamodelName = function(bla){
  datamodel = grep('^DATAMODEL', toupper(bla))
  name = stringr::str_match(bla[datamodel], '^DATAMODEL (.+)$')
  return(name[2])
}

extract_cols = function(bla, group){
  types = c(
    'STRING',
    'REAL',
    'INTEGER'
  )

  regexcols = paste0(
    '^.*',
    ':',
    '(', paste(types, collapse = '|'), ')',
    '\\[.+\\]$'
  )
  cols = grep(regexcols, bla, ignore.case = TRUE)
  groups = stringr::str_match_all(bla[cols], '^(.+):(.+)\\[([,\\d]+)\\]$')
  output = sapply(groups, function(x) x[group])
  return(output)
}

extract_names = function(bla){
  extract_cols(bla, 2)
}

extract_types = function(bla){
  extract_cols(bla, 3)
}

extract_lengths = function(bla){
  extract_cols(bla, 4)
}
