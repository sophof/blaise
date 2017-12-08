read_data = function(datafile, datamodel, locale){
  col_types = convert_types_to_cols(datamodel$col_types, datamodel$col_names)
  readr::read_fwf(
    datafile,
    readr::fwf_widths(
      datamodel$col_lengths,
      datamodel$col_names
    ),
    col_types = col_types,
    locale = locale
  )
}

convert_types_to_cols = function(col_types, col_names){
  col_types = sapply(toupper(col_types), match_type)
  names(col_types) = col_names
  do.call(readr::cols, col_types)
}

match_type = function(type){
  switch(
    EXPR = type,
    STRING = readr::col_character(),
    INTEGER = readr::col_integer(),
    REAL = readr::col_double(),
    stop('type "', type, '" not recognized')
  )
}
