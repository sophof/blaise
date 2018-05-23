write_data = function(df, model, file, decimal.mark = ','){
  uit = create_fixed_width_column(df, model, decimal.mark, digits)
  if (format.info(uit)[1] != sum(variable_widths(model))){
    stop('total output width is not the sum of the individual column widths')
  }
  readr::write_lines(uit, file, na = '\n')
}

# depends on format for doubles
create_fixed_width_column = function(df, model, decimal.mark){
  per_col = function(col, var){
    dig = ifelse(is.na(var@decimals), NULL, var@decimals)
    nas = is.na(col)
    if(is.factor(col)) col = as.integer(col)
    else if(class(col) == 'Date') col = as.character(col)
    else col[!nas] = format(col[!nas], decimal.mark = decimal.mark, digits = dig)

    col = replace_NA(col)
  }

  uit = mapply(per_col, df, variables(model))
  uit = apply(uit, 1, paste, collapse = '')
  return(uit)
}

format_all = function(x, decimal.mark, digits){
  nas = is.na(x)
  c = class(x)
  if(c == 'Date'){
    x = as.character(x)
  }
  else{
    x[!nas] = format(x[!nas], decimal.mark = decimal.mark, digits = digits)
  }
  return(x)
}

replace_NA = function(x){
  nas = is.na(x)
  size = max(nchar(x), na.rm = T)
  if (sum(nas) == length(x)){
    size = 2
  }
  x[nas] = paste(rep(' ', size), collapse = '')
  return(x)
}
