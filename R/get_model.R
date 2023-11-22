get_model = function(df, digits = getOption('digits')){
  vars = mapply(function(col, name){
    if(is.factor(col) & all(stringr::str_detect(levels(col), '^\\d+$'))) col = as.character(col)
    else if(is.logical(col)) col = as.integer(col)
    type = convert_rtype(class(col))
    width = get_width(col, digits)
    decimals = get_decimals(col, digits)
    labels = get_labels(col)
    switch(
      EXPR = type,
      STRING = variable_string(name, width),
      INTEGER =variable_integer(name, width),
      REAL = variable_real(name, width, decimals),
      DATETYPE = variable_date(name),
      ENUM = variable_enum(name, labels = labels),
      DUMMY = variable_dummy(width),
      stop('type "', type, '" not recognized')
      )
  },
  df,
  colnames(df)
  )
  return(model(deparse(substitute(df)), vars))
}

get_width = function(col, digits){
  col = col[!is.na(col)]
  normal = function(col){
    format.info(col, digits = digits)[1]
  }

  date = function(col){
    8
  }

  factor = function(col){
    n = nlevels(col)
    nchar(n)
  }

  real = function(col){
    max(normal(col), 3)
  }

  string = function(col){
    max(nchar(col), 1)
  }

  switch(
    EXPR = class(col)[1],
    character = string(col),
    Date = date(col),
    factor = factor(col),
    numeric = real(col),
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
