read_data = function(datafile,
                     datamodel,
                     locale){
  col_types = convert_types_to_cols(datamodel)
  positions = get_positions(datamodel)
  df = readr::read_fwf(
    datafile,
    positions,
    col_types = col_types,
    locale = locale
  )

  df = convert_factors(df, datamodel)
  return(df)
}

get_positions = function(datamodel){
  widths = variable_widths(datamodel)
  start = c(1, cumsum(widths[1:length(widths) - 1]) + 1)
  names(start) = names(widths)
  end = start + widths - 1
  out = Map(function(a, b) c(a, b), start, end)
  names(out) = variable_names(datamodel)
  out = out[variable_types(datamodel) != 'DUMMY']
  return(do.call(readr::fwf_cols, out))
}

convert_factors = function(df, datamodel){
  mask = variable_types(datamodel) == 'ENUM'
  mask = mask[variable_types(datamodel) != 'DUMMY'] # required because DUMMY types are in datamodel but not in df
  if(!any(mask)) return(df)

  per_factor = function(col, labels, name){
    if (is.numbered_enum(labels)) l = labels # this will read numbered enums correctly
    else l = 1:length(labels)
    if(any(!(unique(na.omit(col)) %in% l))){
      missing = unique(na.omit(col))[!(unique(na.omit(col)) %in% l)]
      msg = sprintf('integer(s) "%s" have no associated label for variable %s',
                    paste(missing, collapse = ';'),
                    name)
      stop(msg)
    }
    factor(col,
           levels = l,
           labels = labels)
  }
  df[,mask] = Map(per_factor,
                  df[,mask],
                  variable_labels(datamodel)[mask],
                  variable_names(datamodel)[mask])
  return(df)
}

convert_types_to_cols = function(model){
  col_types = variable_types(model)
  col_types = lapply(col_types, match_type)
  names(col_types) = variable_names(model)
  col_types = col_types[!sapply(col_types, is.null)]
  do.call(readr::cols_only, col_types)
}

match_type = function(type){
  switch(
    EXPR = toupper(type),
    STRING = readr::col_character(),
    INTEGER = readr::col_integer(),
    REAL = readr::col_double(),
    DATETYPE = readr::col_date(format = '%Y%m%d'),
    ENUM = readr::col_integer(),
    DUMMY = NULL,
    stop('type "', type, '" not recognized')
  )
}
