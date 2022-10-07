#' @include generics.R model.R
NULL

read_data_laf = function(datafile,
                         datamodel,
                         locale,
                         numbered_enum){
  laf_dm = make_laf_dm(datafile, datamodel, locale)
  l = list()
  for (var in variables(datamodel)){
    if(inherits(var, "variable_enum")){
      if(is.numbered_enum(var) & numbered_enum)
        lab = variable_levels(var)
      else
        lab = variable_labels(var)
      l[[name(var)]] = data.frame(
        levels = variable_levels(var),
        labels = lab)
      laf_dm$columns[laf_dm$columns$name == name(var), 'type'] = 'integer'
    }
  }
  laf_dm$levels = l
  return(LaF::laf_open(laf_dm))
}

make_laf_dm = function(datafile, datamodel, locale){
  cols = data.frame(
    name = model_names(datamodel),
    type = sapply(model_types(datamodel), convert_laftype),
    width = model_widths(datamodel),
    stringsAsFactors = FALSE
  )
  list(
    type = "fwf",
    filename = datafile,
    dec = locale$decimal_mark,
    columns = cols
  )
}
