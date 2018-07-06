write_datamodel = function(model, filepath, name = NULL){
  maxwidth = max(nchar(variable_names(model)))

  fields = Map(make_field,
               variable_names(model),
               maxwidth,
               variable_types(model),
               variable_widths(model),
               variable_decimals(model),
               variable_labels(model))
  header = paste0('DATAMODEL ', name, '\nFIELDS')
  content = paste(fields, collapse = '\n')
  footer = 'ENDMODEL'
  text = paste(header, content, footer, sep = '\n')
  readr::write_file(text, filepath)
}

make_field = function(name, max_char, type, width, dec = NA, levels = NA){
  name = format(name, width = max_char)
  width = as.character(width)

  # if a decimal is given, change [] part to incorporate it
  if (!is.na(dec)){
    paste0(width, ',', as.character(dec))
  }

  if (type == 'ENUM'){
    tabwidth = nchar(sprintf('  %s : (', name))
    field = sprintf('  %s : (%s)',
                    name,
                    paste(levels,
                          collapse = sprintf(',\n%s', empty_line(tabwidth))))
  }
  else if (type == 'DATETYPE'){
    field = sprintf('  %s : %s',
                    name,
                    type)
  }
  else {
    field = sprintf('  %s : %s[%s]',
                    name,
                    type,
                    width)
  }
}

empty_line = function(nchars){
  paste(replicate(nchars, ' '), collapse = '')
}
