write_data = function(df, model, file, decimal.mark = '.', justify = 'right'){
  uit = create_fixed_width_column(df, model, decimal.mark, justify)
  if (format.info(uit)[1] != sum(model_widths(model))){
    stop('total output width is not the sum of the individual column widths')
  }

  readr::write_lines(uit, file, na = '\n', sep = "\r\n")
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
        col[!nas] = format(col[!nas], width = width(var))
      }
    }

    # Dates
    else if(class(col) == 'Date') col = format(col, format = '%Y%m%d')

    # Doubles with specific decimals
    else if (type(var) == "REAL" & !is.na(decimals(var))){
      info = format.info(col[!nas])
      if(info[2] > decimals(var) | info[1] > width(var)){
        message('reducing significance for variable ',
                name(var),
                ' since the datamodel requires less significance')
        col = round(col, decimals(var))
      }
      if (width(var) == 1) stop("width can not be 1 or smaller for REAL:", name(var))
      col[!nas] = format(round(col[!nas], decimals(var)),
                         decimal.mark = decimal.mark,
                         digits = width(var) - 1,
                         width = width(var),
                         nsmall = decimals(var),
                         scientific = FALSE)
    }

    # Doubles with no specific decimals
    else if (type(var) == "REAL" & is.na(decimals(var))){
      info = format.info(col[!nas])
      if(info[1] > width(var)) {
        message('reducing significance for variable ',
                name(var),
                ' since the datamodel requires less significance')
        col = signif(col, width(var) - 1)
      }
      if (width(var) == 1) stop("width can not be 1 or smaller for REAL:", name(var))
      col[!nas] = format(col[!nas],
                         decimal.mark = decimal.mark,
                         width = width(var),
                         digits = width(var) - 1,
                         scientific = FALSE)
    }

    # logicals
    else if(is.logical(col)){
      message('variable ',
              name(var),
              ' is automatically converted from logical to integer')
      col = as.integer(col)
      col[!nas] = format(col[!nas], width = width(var))
    }

    # The rest
    else col[!nas] = format(col[!nas],
                            decimal.mark = decimal.mark,
                            width = width(var),
                            justify = justify,
                            scientific = FALSE)

    col = replace_NA(col, width(var))
    nmax = format.info(col)[1]
    if(width(var) < nmax){
      stop('width in datamodel smaller than number of characters of largest element for variable: ',
           name(var))
    }
    if(width(var) > nmax){
      stop('width in datamodel larger than number of characters of largest element for variable: ',
           name(var))
    }
    return(col)
  }

  uit = as.data.frame(mapply(per_col, df, variables(model), SIMPLIFY = FALSE))
  uit = apply(uit, 1, paste, collapse = '')
  return(uit)
}

replace_NA = function(x, width){
  x[is.na(x)] = paste(rep(' ', width), collapse = '')
  return(x)
}
