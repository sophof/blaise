write_data = function(df, model, file, decimal.mark = ',', justify = 'right'){
  uit = create_fixed_width_column(df, model, decimal.mark, justify)
  if (format.info(uit)[1] != sum(variable_widths(model))){
    stop('total output width is not the sum of the individual column widths')
  }

  readr::write_lines(uit, file, na = '\n')
}

# depends on format for doubles
create_fixed_width_column = function(df, model, decimal.mark, justify){
  per_col = function(col, var){
    nas = is.na(col)
    if(is.factor(col)) {
      col = as.integer(col)
      col[!nas] = format(col[!nas], width = var@width)
    }
    else if(class(col) == 'Date') col = as.character.Date(col, format = '%Y%m%d')
    else col[!nas] = format(col[!nas], decimal.mark = decimal.mark, width = var@width, justify = justify)

    col = replace_NA(col, var@width)
    if(var@width < max(nchar(col))){
      stop('width in datamodel smaller than number of characters of largest element for variable: ',
           name(var))
    }
    return(col)
  }

  uit = mapply(per_col, df, variables(model))
  uit = apply(uit, 1, paste, collapse = '')
  return(uit)
}

replace_NA = function(x, width){
  x[is.na(x)] = paste(rep(' ', width), collapse = '')
  return(x)
}
