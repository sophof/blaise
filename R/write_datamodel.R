write_datamodel = function(df, formatinfo, filepath, force_string = FALSE){
  maxwidth = max(nchar(formatinfo$names))

  fields = Map(make_field,
               df,
               formatinfo$names,
               maxwidth,
               formatinfo$types,
               formatinfo$widths,
               formatinfo$decs,
               formatinfo$levels)
  header = 'DATAMODEL\nFIELDS'
  content = paste(fields, collapse = '\n')
  footer = 'ENDMODEL'
  text = paste(header, content, footer, sep = '\n')
  readr::write_file(text, filepath)
}

make_field = function(df, name, max_char, type, width, dec = NA, levels = NA){
  name = format(name, width = max_char)
  width = as.character(width)

  # if a decimal is given, change [] part to incorporate it
  if (!is.na(dec)){
    paste0(width, ',', as.character(dec))
  }

  type = convert_type(type, df)

  if (type == 'ENUM'){
    tabwidth = nchar(sprintf('  %s : (', name))
    field = sprintf('  %s : (%s)',
                    name,
                    paste(levels,
                          collapse = sprintf(',\n%s', empty_line(tabwidth))))
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
