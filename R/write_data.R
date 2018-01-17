write_data = function(df, formatinfo, file, decimal.mark = ',', digits = 7){
  uit = create_fixed_width_column(df, decimal.mark, digits)
  if (format.info(uit)[1] != sum(formatinfo$widths)){
    stop('total output width is not the sum of the individual column widths')
  }
  readr::write_lines(uit, file, na = '\n')
}

# depends on format for doubles
create_fixed_width_column = function(df, decimal.mark, digits){
  factors = vapply(df, is.factor, TRUE)
  df = dplyr::mutate_if(df, factors, replace_factors)
  df = dplyr::mutate_all(df, format_all, decimal.mark, digits)
  df = dplyr::mutate_all(df, replace_NA)
  apply(df, 1, paste, collapse = '')
}

replace_factors = function(x){
  as.integer(x)
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
