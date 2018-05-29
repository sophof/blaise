# Convert a dataframe to conform to a given blaise datamodel
#
# Will try and match columns with the fields in the datamodel by name using
# Levenshtein distance.
# Reorder columns as neccesary and convert column types as necessary.
convert_df = function(df, model, max.distance = 0L){
  names_df = colnames(df)
  locations = find_names(names_df, model, max.distance)
  df = df[,locations, drop = FALSE]
  zonder_dummy = sapply(variables(model), type) != 'DUMMY'

  # Convert logicals first to integer since the logical type doesn't exist for blaise
  df = dplyr::mutate_if(df, is.logical, as.integer)

  if(ncol(df) != length(variables(model)[zonder_dummy])) {
    stop("dataframe doesn't have the same numer of columns as datamodel")
  }

  cast_funs = mapply(cast_type, sapply(variables(model)[zonder_dummy], type), sapply(df, class))

  # Turns out to be faster than a vector apply
  incorrect_type = sapply(df, function(x) convert_rtype(class(x))) !=
    variable_types(model)[zonder_dummy]
  for (i in 1:length(cast_funs)){
    if(incorrect_type[i]) df[,i] = cast_funs[[i]](df[,i])
  }

  #insert dummy columns
  for(dummy in dummys(model)){
    DUMMY = paste(rep(' ', width(dummy)), collapse = '')
    df = tibble::add_column(df, DUMMY, .before = location(dummy))
    names(df)[location(dummy)] = NA_character_
  }
  return(df)
}

find_names = function(names, model, max.distance){
  LD = adist(names, as.character(na.omit(variable_names(model))))
  mins = apply(LD, 2, min)
  mins_loc = apply(LD, 2, which.min)
  matches = apply(LD, 2, function(x) sum(x == min(x)))

  if(any(mins > max.distance)){
    msg = paste0(
      'column(s) "',
      paste(variable_names(model)[mins > max.distance], collapse = '; '),
      '" do not match any names in dataframe')
    stop(msg)
  }

  if(any(matches > 1)){
    msg = paste0(
      'multiple matches with the same distance for "',
      paste(names(matches)[matches > 1], collapse = '; '),
      '"')
    stop(msg)
  }

  if(any(table(mins_loc) > 1)){
    msg = paste0(
      'columns "',
      paste(names(matches)[table(mins_loc) > 1], collapse = '; '),
      '" match the same column in the dataframe')
    stop(msg)
  }

  return(apply(LD, 2, which.min))
}
