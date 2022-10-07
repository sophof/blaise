#' @include variable.R generics.R model.R
#' @importFrom utils adist
NULL

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

  for (i in 1:ncol(df))
      df[[i]] = cast_type(variables(model)[zonder_dummy][[i]], df[[i]])

  # insert dummy columns
  for(dummy in dummys(model)){
    DUMMY = paste(rep(' ', width(dummy)), collapse = '')
    df = tibble::add_column(df, DUMMY, .before = get_location(dummy))
    names(df)[get_location(dummy)] = name(dummy)
  }
  return(df)
}

find_names = function(names, model, max.distance){
  mnames = as.character(model_names(variables_without_dummys(model)))
  LD = adist(names, mnames, ignore.case = TRUE)
  mins = apply(LD, 2, min)
  mins_loc = apply(LD, 2, which.min)
  matches = apply(LD, 2, function(x) sum(x == min(x)))

  if(any(mins > max.distance)){
    msg = paste0(
      'column(s) "',
      paste(model_names(model)[mins > max.distance], collapse = '; '),
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

cast_type = function(var, original){
  # cast dates to string
  if(inherits(original, 'Date') & type(var) == 'STRING') {
    return(format(original, format = '%Y%m%d'))
  }

  # Numbered ENUMS
  else if(is.numbered_enum(var))
    return(cast_numbered_enum(var, original))

  # Normal ENUMS
  else if(type(var) == 'ENUM'){
    l_original = unique(as.character(original))
    if(all(stringr::str_detect(l_original, '^\\d+$'))) l = 1:length(var@labels)
    else l = var@labels

    if(!all(toupper(l_original) %in% toupper(l))){
      msg = sprintf('numbers in dataframe column (%s) do not correspond to range of indices in model (%s) for variable %s',
                    paste(unique(original), collapse = ';'),
                    paste(l, collapse = ';'),
                    name(var))
      stop(msg)
    }
    return(factor(original, levels = l, labels = var@labels))
  }

  # Convert factor to STRING
  else if(type(var) == 'STRING' & is.factor(original)){
    if(all(stringr::str_detect(levels(original), '^\\d+$'))) return(as.character(original))
    else return(as.integer(original))
  }

  # Convert double to INTEGER
  else if(type(var) == 'INTEGER' & is.double(original)){
    if(any(floor(original) != original))
      stop('column to be converted to INTEGER vector ', name(var), ' contains decimal values')
    else
      return(format(original, scientific = FALSE))
  }

  # all other cases use a generic cast
  else{
    switch(
      EXPR = type(var),
      'STRING' = return(as.character(original)),
      'INTEGER' = return(as.integer(original)),
      'REAL' = return(as.double(original)),
      'DATETYPE' = return(as.Date.character(original, format = '%Y%m%d')),
      'ENUM' = return(as.factor(original)),
      stop('type "', type(var), '" not implemented')
    )
  }
}

cast_numbered_enum = function(var, original){
  if(is.factor(original))
    org_levels = levels(original)
  else
    org_levels = unique(original)

  if(!all(org_levels %in% variable_levels(var)) & !all(org_levels %in% variable_labels(var))){
    msg = sprintf('unique values in dataframe column (%s) do not correspond to indices or labels in model (%s|%s) for variable %s',
                  paste(unique(original), collapse = ';'),
                  paste(variable_levels(var), collapse = ';'),
                  paste(variable_labels(var), collapse = ';'),
                  name(var))
    stop(msg)
  }
  if(all(org_levels %in% variable_levels(var)))
    return(factor(original, levels = variable_levels(var), labels = variable_levels(var)))
  else
    return(factor(original, levels = variable_labels(var), labels = variable_levels(var)))
}
