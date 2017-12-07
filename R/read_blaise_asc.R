#' @import magrittr

read_blaise_asc = function(datafile, modelfile){
  
}

#' @import stringr
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
}
