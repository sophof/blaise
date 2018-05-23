get_model = function(df, digits = getOption('digits'), force_string = FALSE){

  vars = mapply(function(col, name){
    type = convert_rtype(class(col), force_string)
    width = get_width(col, digits)
    decimals = get_decimals(col, digits)
    labels = get_labels(col)
    variable(name, type, width, decimals, labels)
  },
  df,
  colnames(df)
  )
  return(model(NA_character_, vars))
}

get_width = function(col, digits){
  col = col[!is.na(col)]
  normal = function(col){
    format.info(col, digits = digits)[1]
  }

  date = function(col){
    format.info(as.character(col))
  }

  factor = function(col){
    n = nlevels(col)
    nchar(n)
  }

  switch(
    EXPR = class(col)[1],
    Date = date(col),
    factor = factor(col),
    normal(col)
  )
}

get_decimals = function(col, digits){
  type = class(col)[1]
  if(type == 'numeric'){
    format.info(col, digits = digits)[2]
  }
  else{
    NA_integer_
  }
}

get_labels = function(col){
  type = class(col)[1]
  if(type == 'factor'){
    levels(col)
  }
  else{
    NULL
  }
}
