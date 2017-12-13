read_data = function(datafile,
                     datamodel,
                     locale){
  col_types = convert_types_to_cols(datamodel$col_types,
                                    datamodel$col_names)
  df = readr::read_fwf(
    datafile,
    readr::fwf_widths(
      datamodel$col_lengths,
      datamodel$col_names
    ),
    col_types = col_types,
    locale = locale
  )

  factors = datamodel$col_types == 'ENUM'
  df = convert_factors(df, factors, datamodel)
  return(df)
}

convert_factors = function(df, mask, datamodel){
  cols = colnames(df[,mask])
  levels = lapply(cols, function(name) {
    unlist(
      datamodel$col_levels[match(name, datamodel$col_names)]
    )
  })
  df[,mask] = Map(function(col, levels) factor(col, labels = levels),
                  df[,mask],
                  levels)
  return(df)
}

convert_types_to_cols = function(col_types, col_names){
  col_types = Map(match_type, col_types)
  names(col_types) = col_names
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
