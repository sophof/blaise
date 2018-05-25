read_model = function(blafile, force_string = FALSE){
  bla = readr::read_file(blafile)
  bla = parse_bla(bla, force_string)
  return(bla)
}

parse_bla = function(bla, force_string = FALSE){
  bla = clean_model(bla)
  modelname = extract_datamodelName(bla)
  names = extract_names(bla)
  cols = extract_types_and_widths(bla, force_string)
  types = cols$types
  lengths = cols$widths
  decimals = cols$decs
  labels = cols$levels

  stopifnot(
    all(!is.na(types)),
    all(!is.na(names)),
    all(!is.na(lengths)),
    all(sapply(c(length(types),
                 length(lengths),
                 length(decimals),
                 length(labels)),
               function (x) x == length(names)))
  )

  vars = mapply(function(name, type, length, decimals, labels){
    variable(name, type, length, decimals, labels)
  },
  names,
  types,
  lengths,
  decimals,
  labels)

  m = model(modelname, vars)
  return(m)
}

extract_datamodelName = function(bla){
  datamodel = grep('^DATAMODEL', toupper(bla))
  name = stringr::str_match(bla[datamodel], '^DATAMODEL (.+)$')
  return(name[2])
}

extract_cols = function(bla, group){
  regexcols = paste0(
    '^.*',
    ':',
    '.*',
    '(\\[.+\\])?$'
  )
  cols = grep(regexcols, bla, ignore.case = TRUE)
  groups = stringr::str_match_all(bla[cols], '^(.+):(.+)$')
  output = sapply(groups, function(x) x[group])
  return(output)
}

extract_names = function(bla){
  extract_cols(bla, 2)
}

extract_types_and_widths = function(bla, force_string = FALSE){
  types = c(
    'STRING',
    'REAL',
    'INTEGER',
    'DATETYPE',
    'ENUM'
  )
  cols = extract_cols(bla, 3)
  ret = list(input = cols)

  normal_regex = '^(.+)\\[(\\d+),?(\\d+)?\\]$'
  normal_types = stringr::str_detect(cols, normal_regex)
  ret = extract_normal(ret, normal_types, normal_regex)

  enum_regex = '^\\((.+)\\)$'
  enum_types = str_detect(cols, enum_regex)
  ret = extract_enum(ret, enum_types)

  range_regex = '^(\\d+\\.?(?:\\d+)?)\\.\\.(\\d+\\.?(?:\\d+)?)$'
  range_types = stringr::str_detect(cols, range_regex)
  ret = extract_range(ret, range_types, range_regex)

  date_regex = stringr::regex('^DATETYPE(\\[8\\])?$', ignore_case = TRUE)
  date_types = stringr::str_detect(cols, date_regex)
  ret = extract_date(ret, date_types)

  if (force_string){
    ret$types = replicate(length(ret$types), 'STRING')
  }
  if(any(is.null(ret$types)) | any(is.null(ret$widths))){
    stop('not all datatypes could be detected, model probably malformed')
  }

  ret$types = toupper(ret$types)

  controle = is.na(ret$types) | !(ret$types %in% types)
  if(any(controle)){
    msg = sprintf(
      'datamodel contains unknown type(s) "%s"',
      paste(cols[controle], collapse = '; ')
    )
    stop(msg)
  }
  ret$widths = as.integer(ret$widths)
  if(is.null(ret$decs)) {
    ret$decs = replicate(length(ret$types), NA_integer_)
  }
  else {
    ret$decs = as.integer(ret$decs)
  }

  if(is.null(ret$levels)) ret$levels = lapply(ret$types, function(x) NULL)

  return(ret)
}

extract_normal = function(all, mask, regex){
  if(all(!mask)) return(all) # if none match, return input
  cols = all$input

  all$types[mask] = str_match(cols[mask], regex)[,2]
  all$widths[mask] = str_match(cols[mask], regex)[,3]
  all$decs[mask] = str_match(cols[mask], regex)[,4]

  max_int = nchar(.Machine$integer.max)
  big_int = (toupper(all$types[mask]) == 'INTEGER') &
    (as.integer(all$widths[mask]) >= max_int)
  if (any(big_int)){
    msg = sprintf('column(s) too wide for 32bit int with width "%s", converting to double.
                  max int is %i',
                  paste(all$widths[big_int], collapse = '; '),
                  .Machine$integer.max)
    warning(msg)
    all$types[big_int] = 'REAL'
  }

  max_digits = .Machine$double.digits
  big_float = (toupper(all$types[mask]) == 'REAL') &
    (as.integer(all$widths[mask]) >= max_digits)
  if (any(big_float)){
    msg = sprintf('column(s) too wide for double with width "%s", converting to string.
                  max float digits are %i',
                  paste(all$widths[big_float], collapse = '; '),
                  max_digits)
    warning(msg)
    all$types[big_float] = 'STRING'
  }

  return(all)
}

extract_enum = function(all, mask){
  get_enum_levels = function(string){
    # remove ()
    string = stringr::str_match(string, '\\((.+)\\)')[,2]
    trimws(unlist(stringr::str_split(string, ',')))
  }

  get_enum_size = function(string){
    levels = get_enum_levels(string)
    n = length(levels)
    nchar(n)
  }
  if(all(!mask)) return(all) # if none match, return input
  cols = all$input

  all$types[mask] = 'ENUM'
  all$widths[mask] = vapply(cols[mask], get_enum_size, 1)
  all$levels[mask] = lapply(cols[mask], get_enum_levels)
  return(all)
}

extract_range = function(all, mask, regex){
  if(all(!mask)) return(all) # if none match, return input

  cols = all$input
  start = str_match(cols[mask], regex)[,2]
  end = str_match(cols[mask], regex)[,3]

  startdecs = nchar(stringr::str_match(start, '^\\d+\\.(\\d+)$')[,2])
  enddecs = nchar(stringr::str_match(end, '^\\d+\\.(\\d+)$')[,2])
  decs = pmax(startdecs, enddecs)

  doubles = (str_detect(start, '\\.') | str_detect(end, '\\.'))
  all$types[mask][doubles] = 'REAL'
  all$types[mask][!doubles] = 'INTEGER'

  all$decs[mask][doubles] = decs[doubles]

  all$widths[mask] = mapply(function(start, end) {
    max(nchar(start), nchar(end))
    },
    start,
    end,
    USE.NAMES = F)

  return(all)
}

extract_date = function(all, mask){
  if(all(!mask)) return(all) #if none match return input

  all$types[mask] = 'DATETYPE'
  all$widths[mask] = 8L
  return(all)
}
