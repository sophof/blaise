read_data = function(datafile,
                     datamodel,
                     locale){
  col_types = convert_types_to_cols(datamodel)
  df = readr::read_fwf(
    datafile,
    readr::fwf_widths(
      variable_widths(datamodel),
      variable_names(datamodel)
    ),
    col_types = col_types,
    locale = locale
  )

  df = convert_factors(df, datamodel)
  return(df)
}

convert_factors = function(df, datamodel){
  mask = variable_types(datamodel) == 'ENUM'
  df[,mask] = Map(function(col, labels) factor(col, labels = labels),
                  df[,mask],
                  variable_labels(datamodel)[mask])
  return(df)
}

convert_types_to_cols = function(model){
  col_types = variable_types(model)
  col_types = Map(match_type, col_types)
  names(col_types) = variable_names(model)
  do.call(readr::cols, col_types)
}

match_type = function(type){
  switch(
    EXPR = toupper(type),
    STRING = readr::col_character(),
    INTEGER = readr::col_integer(),
    REAL = readr::col_double(),
    DATETYPE = readr::col_date(),
    ENUM = readr::col_integer(),
    stop('type "', type, '" not recognized')
  )
}
