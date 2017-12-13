write_data = function(df, formatinfo, file, decimal.mark = ',', digits = 7){
  df = replace_factors(df)
  uit = create_fixed_width_column(df, decimal.mark, digits)
  if (format.info(uit)[1] != sum(formatinfo$widths)){
    stop('total output width is not the sum of the individual column widths')
  }
  readr::write_lines(uit, file, na = '\n')
}

# depends on format for doubles
create_fixed_width_column = function(df, decimal.mark, digits){
  df = replace_factors(df)
  df = dplyr::mutate_all(df, format, decimal.mark = decimal.mark, digits = digits)
  apply(df, 1, paste, collapse = '')
}

replace_factors = function(df){
  factors = vapply(df, is.factor, TRUE)
  dplyr::mutate_if(df, factors, as.integer)
}
