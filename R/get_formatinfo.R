get_formatinfo = function(df, digits = 7){
  ret = list(names = colnames(df))
  ret$types = vapply(df, class, '')
  ret$widths = vapply(df, get_width, 1, digits)
  ret$decs = vapply(df, get_dec, 1, digits)
  ret$levels = lapply(df, get_levels)

  stopifnot(same_length(ret$types,
                        ret$names,
                        ret$widths,
                        ret$decs))
  return(ret)
}

get_width = function(col, digits){
  normal = function(col){
    format.info(col, digits = digits)[1]
  }

  date = function(col){
    format.info(as.character(col))
  }

  factor = function(col){
    n = length(levels(col))
    nchar(n)
  }

  switch(
    EXPR = class(col)[1],
    Date = date(col),
    factor = factor(col),
    normal(col)
  )
}

get_dec = function(col, digits){
  type = class(col)[1]
  if(type == 'numeric'){
    format.info(col, digits = digits)[2]
  }
  else{
    NA
  }
}

get_levels = function(col){
  type = class(col)[1]
  if(type == 'factor'){
    levels(col)
  }
  else{
    NA
  }
}

same_length = function(vector, ...){
  others = list(...)
  N = length(vector)
  all(sapply(others, length) == N)
}
