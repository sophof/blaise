write_datamodel = function(df, formatinfo, filepath, force_string = FALSE){
  types = vapply(df, class, '')
  decs = replace(formatinfo$digits, formatinfo$digits == 0, NA)
  names = formatinfo$colname
  widths = formatinfo$width

  stopifnot(same_length(types, names, widths, decs))

  fields = Map(make_field,
               df,
               names,
               max(nchar(names)),
               types,
               widths,
               decs)
  header = 'DATAMODEL\nFIELDS'
  content = paste(fields, collapse = '\n')
  footer = 'ENMODEL'
  text = paste(header, content, footer, sep = '\n')
  readr::write_file(text, filepath)
}

make_field = function(df, name, max_char, type, width, dec = NA){
  name = format(name, width = max_char)
  width = as.character(width)

  # if a decimal is given, change width to incorporate it
  if (!is.na(dec)){
    paste0(width, ',', as.character(dec))
  }

  type = convert_type(type, df)

  field = sprintf('  %s : %s[%s]',
                  name,
                  type,
                  width)
}

convert_type = function(type, df, force_string = FALSE){
  switch(
    EXPR = type,
    'character' = 'STRING',
    'integer' = 'INTEGER',
    'numeric' = 'REAL',
    'Date' = 'DATETYPE',
    'factor' = 'STRING',
    if (force_string) 'STRING'
    else stop('type "', type, '" not implemented')
  )
}

same_length = function(vector, ...){
  others = list(...)
  N = length(vector)
  all(sapply(others, length) == N)
}
