write_data = function(df, model, file, decimal.mark = '.', justify = 'right'){
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

    # Factors
    if(is.factor(col)) {
      if (is.numbered_enum(levels(col))){
        col = as.character(col)
        col[!nas] = format(col[!nas], width = width(var), justify = justify)
      }
      else {
        col = as.integer(col)
        col[!nas] = format(col[!nas], width = var@width)
      }
    }

    # Dates
    else if(class(col) == 'Date') col = as.character.Date(col, format = '%Y%m%d')

    # Doubles with specific decimals
    else if (is.numeric(col) & !is.na(var@decimals)){
      info = format.info(col[!nas])
      if(info[2] > var@decimals | info[1] > var@width){
        message('reducing significance for variable ',
                name(var),
                ' since the datamodel requires less significance')
        col = round(col, var@decimals)
      }
      col[!nas] = format(round(col[!nas], var@decimals),
                         decimal.mark = decimal.mark,
                         digits = var@width - 1,
                         width = var@width,
                         nsmall = var@decimals,
                         scientific = FALSE)
    }

    # Doubles with no specific decimals
    else if (is.numeric(col) & is.na(var@decimals)){
      info = format.info(col[!nas])
      if(info[1] > var@width) {
        message('reducing significance for variable ',
                name(var),
                ' since the datamodel requires less significance')
        col = signif(col, var@width - 1)
      }
      col[!nas] = format(col[!nas],
                         decimal.mark = decimal.mark,
                         width = var@width,
                         digits = var@width - 1,
                         scientific = FALSE)
    }

    # logicals
    else if(is.logical(col)){
      message('variable ',
              name(var),
              ' is automatically converted from logical to integer')
      col = as.integer(col)
      col[!nas] = format(col[!nas], width = var@width)
    }

    # The rest
    else col[!nas] = format(col[!nas],
                            decimal.mark = decimal.mark,
                            width = var@width,
                            justify = justify,
                            scientific = FALSE)

    col = replace_NA(col, var@width)
    nmax = format.info(col)[1]
    if(width(var) < nmax){
      stop('width in datamodel smaller than number of characters of largest element for variable: ',
           name(var))
    }
    if(var@width > nmax){
      stop('width in datamodel larger than number of characters of largest element for variable: ',
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
