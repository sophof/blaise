# Convert a dataframe to conform to a given blaise datamodel
#
# Will try and match columns with the fields in the datamodel by name using
# Levenshtein distance.
# Reorder columns as neccesary and convert column types as necessary.
convert_df = function(df, model, max.distance = 4L){
  names_df = colnames(df)
  locations = find_names(names_df, model, max.distance)
  df = df[,locations]

  if(ncol(df) != length(variables(model))) {
    stop("dataframe doesn't have the same numer of columns as datamodel")
  }

  cast_funs = lapply(sapply(variables(model), type), cast_type)
  df = mapply(function(col, var) cast_type(type(var))(col), df, variables(model))
  return(df)
}

find_names = function(names, model, max.distance){
  LD = adist(names, variable_names(model))
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
