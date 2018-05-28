read_model = function(blafile, force_string = FALSE){
  bla = readr::read_file(blafile)
  bla = parse_bla(bla, force_string)
  return(bla)
}

parse_bla = function(bla, force_string = FALSE){
  bla = clean_model(bla)
  modelname = extract_datamodelName(bla)
  bla = remove_non_fields(bla)
  names = extract_names(bla)
  types = extract_types(bla, force_string)
  widths = extract_widths(bla)
  decimals = get_real_decimals(bla)
  labels = get_enum_labels(bla)

  #check if any widths are too big for the type to support
  types = convert_ints_and_reals(names, types, widths)

  stopifnot(
    all(!is.na(types)),
    all(!is.na(widths)),
    all(sapply(c(length(types),
                 length(widths),
                 length(decimals),
                 length(labels),
                 length(bla)),
               function (x) x == length(names)))
  )

  vars = mapply(function(name, type, width, decimals, labels){
    variable(name, type, width, decimals, labels)
  },
  names,
  types,
  widths,
  decimals,
  labels)

  m = model(modelname, vars)
  return(m)
}

remove_non_fields = function(bla){
  modelname = stringr::str_detect(bla, '^DATAMODEL')
  FIELDS = stringr::str_detect(bla, 'FIELDS')
  ENDMODEL = stringr::str_detect(bla, 'ENDMODEL')
  bla[!(modelname | ENDMODEL | FIELDS)]
}

extract_datamodelName = function(bla){
  datamodel = grep('^DATAMODEL', toupper(bla))
  name = stringr::str_match(bla[datamodel], '^DATAMODEL (.+)$')
  return(name[2])
}

extract_names = function(bla){
  stringr::str_match(bla, '^(\\S):.+$')[,2]
}

extract_types = function(bla, force_string = FALSE){
  haakjes_regex = '^.+:(.+)\\[(\\d+),?(\\d+)?\\]$'
  types = stringr::str_match(bla, haakjes_regex)[,2]

  types = rep(NA_character_, length(bla))
  types[detect_strings(bla)] = 'STRING'
  types[detect_ints(bla)] = 'INTEGER'
  types[detect_enums(bla)] = 'ENUM'
  types[detect_reals(bla)] = 'REAL'
  types[detect_dates(bla)] = 'DATETYPE'
  types[detect_dummys(bla)] = 'DUMMY'

  if(any(is.na(types))) stop(sprintf('"%s" could not be detected as type',
                                     paste(bla[is.na(types)], collapse = ';')))
  return(types)
}

extract_widths = function(bla){
  widths = rep(NA_integer_, length(bla))

  widths = fill_vector(widths, get_string_widths(bla))
  widths = fill_vector(widths, get_int_widths(bla))
  widths = fill_vector(widths, get_real_widths(bla))
  widths = fill_vector(widths, get_enum_widths(bla))
  widths = fill_vector(widths, get_date_widths(bla))
  widths = fill_vector(widths, get_dummy_widths(bla))

  if(any(is.na(widths))) stop(sprintf(' width could not be detected for "%s"',
                                     paste(bla[is.na(widths)], collapse = ';')))

  return(widths)
}

detect_strings = function(bla){
  regex = stringr::regex('^.+:STRING\\[\\d+\\]$', ignore_case = TRUE)
  stringr::str_detect(bla, regex)
}

get_string_widths = function(bla){
  if(all(!detect_strings(bla))) return(rep(NA_integer_, length(bla)))

  regex = stringr::regex('^.+:STRING\\[(\\d+)\\]$', ignore_case = TRUE)
  as.integer(stringr::str_match(bla, regex)[,2])
}


detect_ints = function(bla){
  haakjes_regex = stringr::regex('^.+:INTEGER\\[\\d+\\]$', ignore_case = TRUE)
  haakjes = stringr::str_detect(bla, haakjes_regex)

  range_regex = '^.+:\\d+\\.\\.\\d+$'
  ranges = stringr::str_detect(bla, range_regex)
  return(ranges | haakjes)
}

get_int_widths = function(bla){
  if(all(!detect_ints(bla))) return(rep(NA_integer_, length(bla)))

  widths = rep(NA_integer_, length(bla))
  haakjes_regex = stringr::regex('^.+:INTEGER\\[(\\d+)\\]$', ignore_case = TRUE)
  haakjes = stringr::str_match(bla, haakjes_regex)[,2]
  widths = fill_vector(widths, haakjes)

  range_regex = '^.+:(\\d+)\\.\\.(\\d+)$'
  start = nchar(stringr::str_match(bla, range_regex)[,2])
  end = nchar(stringr::str_match(bla, range_regex)[,3])
  ranges = pmax(start, end)
  widths = fill_vector(widths, ranges)
  return(as.integer(widths))
}

detect_reals = function(bla){
  haakjes_regex = stringr::regex('^.+:REAL\\[\\d+,\\d+\\]$', ignore_case = TRUE)
  haakjes = stringr::str_detect(bla, haakjes_regex)

  range_regex = '^\\d+\\.\\d+\\.\\.\\d+\\.\\d+$'
  ranges = stringr::str_detect(bla, range_regex)
  return(ranges | haakjes)
}

get_real_widths = function(bla){
  if(all(!detect_reals(bla))) return(rep(NA_integer_, length(bla)))

  widths = rep(NA_integer_, length(bla))
  haakjes_regex = stringr::regex('^.+:REAL\\[(\\d+),\\d+\\]$', ignore_case = TRUE)
  haakjes = stringr::str_match(bla, haakjes_regex)[,2]
  widths = fill_vector(widths, haakjes)

  range_regex = '^(\\d+\\.\\d+)\\.\\.(\\d+\\.\\d+)$'
  start = nchar(stringr::str_match(bla, range_regex)[,2])
  end = nchar(stringr::str_match(bla, range_regex)[,3])
  ranges = pmax(start, end)
  widths = fill_vector(widths, ranges)
  return(as.integer(widths))
}

get_real_decimals = function(bla){
  if(all(!detect_reals(bla))) return(rep(NA_integer_, length(bla)))

  decimals = rep(NA_integer_, length(bla))
  haakjes_regex = stringr::regex('^.+:REAL\\[\\d+,(\\d+)\\]$', ignore_case = TRUE)
  haakjes = stringr::str_match(bla, haakjes_regex)[,2]
  decimals = fill_vector(decimals, haakjes)

  range_regex = '^\\d+\\.(\\d+)\\.\\.\\d+\\.(\\d+)$'
  startdecs = nchar(stringr::str_match(start, range_regex)[,2])
  enddecs = nchar(stringr::str_match(end, range_regex)[,3])
  decimals = fill_vector(widths, pmax(startdecs, enddecs))

  return(as.integer(decimals))
}


detect_enums = function(bla){
  enum_regex = '^.+:\\((.+)\\)$'
  stringr::str_detect(bla, enum_regex)
}

get_enum_labels = function(bla){
  if(all(!detect_enums(bla))) return(rep(NA_character_, length(bla)))
  # remove ()
  per_enum = function(string){
    string = stringr::str_match(string, '\\((.+)\\)')[,2]
    trimws(unlist(stringr::str_split(string, ',')))
  }
  return(lapply(bla, per_enum))
}

get_enum_widths = function(bla){
  if(all(!detect_enums(bla))) return(rep(NA_integer_, length(bla)))
  per_enum = function(string){
    levels = get_enum_labels(string)
    n = length(levels)
    nchar(n)
  }
  return(as.integer(lapply(bla, per_enum)))
}

detect_dates = function(bla){
  date_regex = stringr::regex('^.+:DATETYPE(?:\\[8\\])?$', ignore_case = TRUE)
  stringr::str_detect(bla, date_regex)
}

get_date_widths = function(bla){
  widths = rep(NA_integer_, length(bla))
  widths[detect_dates(bla)] = 8L
  return(widths)
}

detect_dummys = function(bla){
  dummy_regex = stringr::regex('^DUMMY\\[\\d+\\]$', ignore_case = TRUE)
  stringr::str_detect(bla, dummy_regex)
}

get_dummy_widths = function(bla){
  if(all(!detect_dummys(bla))) return(rep(NA_integer_, length(bla)))

  dummy_regex = stringr::regex('^DUMMY\\[(\\d+)\\]$', ignore_case = TRUE)
  as.integer(stringr::str_match(bla, dummy_regex)[,2])
}

convert_ints_and_reals = function(names, types, widths){
  #ints
  max_int = nchar(.Machine$integer.max)
  big_int = (types == 'INTEGER') &
    (widths >= max_int)

  if (any(big_int)){
    msg = sprintf('column(s) "%s" too wide for 32bit int with width "%s", converting to double.
                  max int is %i',
                  paste(names[big_int], collapse = '; '),
                  paste(widths[big_int], collapse = '; '),
                  max_int)
    warning(msg)
    types[big_int] = 'REAL'
  }

  max_digits = .Machine$double.digits
  big_float = (types == 'REAL') &
    (widths >= max_digits)
  if (any(big_float)){
    msg = sprintf('column(s) "%s" too wide for double with width "%s", converting to string.
                  max float digits are %i',
                  paste(names[big_float], collapse = '; '),
                  paste(widths[big_float], collapse = '; '),
                  max_digits)
    warning(msg)
    types[big_float] = 'STRING'
  }
  return(types)
}
