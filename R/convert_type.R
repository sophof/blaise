convert_rtype = function(type, force_string = FALSE){
  switch(
    EXPR = type,
    'character' = 'STRING',
    'integer' = 'INTEGER',
    'numeric' = 'REAL',
    'Date' = 'DATETYPE',
    'factor' = 'ENUM',
    if (force_string) 'STRING'
    else stop('type "', type, '" not implemented')
  )
}

convert_type = function(type){
  switch(
    EXPR = type,
    'STRING' = 'character',
    'INTEGER' = 'integer',
    'REAL' = 'numeric',
    'DATETYPE' = 'Date',
    'ENUM' = 'factor',
    stop('type "', type, '" not implemented')
  )
}

cast_type = function(type){
  switch(
    EXPR = type,
    'STRING' = as.character,
    'INTEGER' = as.integer,
    'REAL' = as.numeric,
    'DATETYPE' = as.Date,
    'ENUM' = as.factor,
    stop('type "', type, '" not implemented')
  )
}
