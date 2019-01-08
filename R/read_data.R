#' @include variable.R generics.R model.R
NULL

read_data = function(datafile,
                     datamodel,
                     locale,
                     numbered_enum = TRUE){
  col_types = convert_types_to_cols(datamodel)
  positions = get_positions(datamodel)

  df = readr::read_fwf(
    datafile,
    positions,
    col_types = col_types,
    locale = locale
  )

  df = convert_factors(df, datamodel, numbered_enum)
  return(df)
}

get_positions = function(datamodel){
  widths = model_widths(datamodel)
  start = c(1, cumsum(widths[1:length(widths) - 1]) + 1)
  names(start) = names(widths)
  end = start + widths - 1
  out = Map(function(a, b) c(a, b), start, end)
  names(out) = model_names(datamodel)

  # If last type is a string, the fwf is usually 'ragged'. allow readr to read
  # this by setting the end position to NA for that column
  if (model_types(datamodel)[length(out)] == "STRING"){
    out[[length(out)]][2] = NA
  }
  out = out[model_types(datamodel) != 'DUMMY']
  return(do.call(readr::fwf_cols, out))
}

convert_factors = function(df, datamodel, convert_numbered){
  mask = model_types(datamodel) == 'ENUM'
  mask_df = mask[model_types(datamodel) != 'DUMMY'] # required because DUMMY types are in datamodel but not in df
  if(!any(mask)) return(df)

  per_factor = function(col, labels, levels, name, numbered){
    if(any(!(unique(na.omit(col)) %in% levels))){
      missing = unique(na.omit(col))[!(unique(na.omit(col)) %in% levels)]
      msg = sprintf('integer(s) "%s" have no associated label for variable %s',
                    paste(missing, collapse = ';'),
                    name)
      stop(msg)
    }
    if (numbered)
      labels = as.character(levels) # this will read numbered enums correctly
    factor(col,
           levels = levels,
           labels = labels)
  }

  stopifnot(sum(mask) == sum(mask_df))
  numbered = sapply(variables(datamodel)[mask], is.numbered_enum) & convert_numbered
  df[,mask_df] = Map(per_factor,
                  df[,mask_df],
                  model_labels(datamodel)[mask],
                  model_levels(datamodel)[mask],
                  model_names(datamodel)[mask],
                  numbered
                  )
  return(df)
}

convert_types_to_cols = function(model){
  col_types = model_types(model)
  col_types = lapply(col_types, match_type)
  names(col_types) = model_names(model)
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
