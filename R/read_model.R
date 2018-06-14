#' @import stringr
read_model = function(blafile, force_string = FALSE){
  bla = readr::read_file(blafile)
  custom_types = read_custom_types(bla)
  bla = parse_bla(bla, custom_types, force_string)
  return(bla)
}

parse_bla = function(bla, custom_types, force_string = FALSE){
  bla = clean_model(bla)
  modelname = extract_datamodelName(bla)
  bla = remove_non_fields(bla)
  if(length(bla) == 0) stop('No fields found')

  make_var = function(field){
    type = detect_type(field, custom_types)
    switch(type,
           STRING = make_string(field),
           INTEGER = make_int(field),
           REAL = make_real(field),
           ENUM = make_enum(field),
           DATETYPE = make_date(field),
           DUMMY = make_dummy(field),
           CUSTOM = make_custom(field, custom_types),
           UNKNOWN = make_unknown(field, force_string),
           stop('Type could not be detected')
    )
  }

  vars = lapply(bla, make_var)
  m = model(modelname, vars)
  return(m)
}

remove_non_fields = function(bla){
  modelname = str_detect(bla, '^DATAMODEL')
  FIELDS = str_detect(bla, 'FIELDS')
  ENDMODEL = str_detect(bla, 'ENDMODEL')
  bla[!(modelname | ENDMODEL | FIELDS)]
}

extract_datamodelName = function(bla){
  datamodel = grep('^DATAMODEL', toupper(bla))
  name = str_match(bla[datamodel], '^DATAMODEL (.+)$')
  return(name[2])
}

detect_type = function(field, custom_types){
  if(detect_string(field)) return('STRING')
  if(detect_int(field)) return('INTEGER')
  if(detect_real(field)) return('REAL')
  if(detect_enum(field)) return('ENUM')
  if(detect_date(field)) return('DATETYPE')
  if(detect_dummy(field)) return('DUMMY')
  if(detect_custom(field, custom_types)) return('CUSTOM')
  if(detect_unknown(field)) return('UNKNOWN')
}

detect_string = function(field){
  regex = regex('^.+:STRING\\[\\d+\\]$', ignore_case = TRUE)
  str_detect(field, regex)
}

make_string = function(field){
  reg = regex('(^.+):STRING\\[(\\d+)\\]$', ignore_case = TRUE)
  name = str_match(field, reg)[,2]
  width = as.integer(str_match(field, reg)[,3])
  variable(name = name, type = 'STRING', width = width)
}

detect_int = function(field){
  haakjes_regex = regex('^.+:INTEGER\\[\\d+\\]$', ignore_case = TRUE)
  haakjes = str_detect(field, haakjes_regex)

  range_regex = '^.+:\\d+\\.\\.\\d+$'
  ranges = str_detect(field, range_regex)
  return(ranges | haakjes)
}

make_int = function(field){
  haakjes_regex = regex('(^.+):INTEGER\\[(\\d+)\\]$', ignore_case = TRUE)
  if(str_detect(field, haakjes_regex)){
    name = str_match(field, haakjes_regex)[,2]
    width = as.integer(str_match(field, haakjes_regex)[,3])

  }
  else{
    range_regex = '^(.+):(\\d+)\\.\\.(\\d+)$'
    name = str_match(field, range_regex)[,2]
    start = nchar(str_match(field, range_regex)[,3])
    end = nchar(str_match(field, range_regex)[,4])
    width = max(start, end)
  }
  variable(name = name, type = 'INTEGER', width = width)
}

detect_real = function(field){
  haakjes_regex = regex('^.+:REAL\\[\\d+(?:,\\d+)?\\]$', ignore_case = TRUE)
  haakjes = str_detect(field, haakjes_regex)

  range_regex = '^.+:\\d+\\.\\d+\\.\\.\\d+\\.\\d+$'
  ranges = str_detect(field, range_regex)
  return(ranges | haakjes)
}

make_real = function(field){
  haakjes_regex = regex('(^.+):REAL\\[(\\d+)(?:,(\\d+))?\\]$',
                        ignore_case = TRUE)
  if(str_detect(field, haakjes_regex)){
    name = str_match(field, haakjes_regex)[,2]
    width = as.integer(str_match(field, haakjes_regex)[,3])
    decimals = as.integer(str_match(field, haakjes_regex)[,4])
  }
  else{
    range_regex = '^(.+):(\\d+\\.(\\d+))\\.\\.(\\d+\\.(\\d+))$'
    name = str_match(field, range_regex)[,2]

    start = nchar(str_match(field, range_regex)[,3])
    end = nchar(str_match(field, range_regex)[,5])
    width = max(start, end)

    startdecs = nchar(str_match(field, range_regex)[,4])
    enddecs = nchar(str_match(field, range_regex)[,6])
    decimals = max(startdecs, enddecs)
  }
  variable(name = name, type = 'REAL', width = width, decimals = decimals)
}

detect_enum = function(field){
  enum_regex = '^.+:\\((.+)\\)$'
  str_detect(field, enum_regex)
}

make_enum = function(field){
  name = str_match(field, '^(.*):\\(.*\\)$')[,2]
  numbered = FALSE

  labels = str_match(field, '\\((.+)\\)')[,2]
  labels = trimws(unlist(str_split(labels, ',')))
  if (all(str_detect(labels, '^\\w+.*\\(\\s*\\d+\\s*\\)$'))){
    labels = str_match(labels, '\\((\\d+)\\)')[,2]
    numbered = TRUE
  }

  if(numbered) width = max(nchar(labels))
  else width = nchar(length(labels))

  variable(name = name, type = 'ENUM', width = width, labels = labels)
}

detect_date = function(field){
  date_regex = regex('^.+:DATETYPE(?:\\[8\\])?$', ignore_case = TRUE)
  str_detect(field, date_regex)
}

make_date = function(field){
  date_regex = regex('^(.+):DATETYPE(?:\\[8\\])?$', ignore_case = TRUE)
  name = str_match(field, date_regex)[,2]
  variable(name = name, type = 'DATETYPE', width = 8L)
}

detect_dummy = function(field){
  dummy_regex = regex('^DUMMY\\[\\d+\\]$', ignore_case = TRUE)
  str_detect(field, dummy_regex)
}

make_dummy = function(field){
  dummy_regex = regex('^DUMMY\\[(\\d+)\\]$', ignore_case = TRUE)
  width = as.integer(str_match(field, dummy_regex)[,2])
  variable(type = 'DUMMY', width = width)
}

detect_custom = function(field, custom_types){
  if(is.null(custom_types)) return(FALSE)
  types = variable_names(custom_types)
  reg_types = regex(paste(paste0('^.*:', types, '$'), collapse = '|'),
                    ignore_case = TRUE)
  str_detect(field, reg_types)
}

make_custom = function(field, custom_types){
  if(is.null(custom_types)) stop('no custom types available')
  reg_types = regex('^(\\w+):(\\w+)$', ignore_case = TRUE)
  name = str_match(field, reg_types)[,2]
  type = str_match(field, reg_types)[,3]
  custom_type = get_variable(custom_types, type)
  variable(name = name, type = 'ENUM', width = width(custom_type), labels = custom_type@labels)
}

detect_unknown = function(field){
  regex = regex('^\\w+:\\w+\\[\\d+\\]$', ignore_case = TRUE)
  str_detect(field, regex)
}

make_unknown = function(field, force_string){
  if(!force_string) stop('Type could not be detected for ', field)
  reg_unknown = regex('^(\\w+):(\\w+)\\[(\\d+)\\]$')
  name = str_match(field, reg_unknown)[,2]
  type = str_match(field, reg_unknown)[,3]
  width = as.integer(str_match(field, reg_unknown)[,4])
  variable(name = name, type = 'STRING', width = width)
}
