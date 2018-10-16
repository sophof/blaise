convert_rtype = function(type){
  switch(
    EXPR = type,
    'character' = 'STRING',
    'integer' = 'INTEGER',
    'numeric' = 'REAL',
    'Date' = 'DATETYPE',
    'factor' = 'ENUM',
    stop('type "', type, '" not implemented')
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

convert_laftype = function(type){
  switch(
    EXPR = type,
    'STRING' = 'character',
    'INTEGER' = 'integer',
    'REAL' = 'numeric',
    'DATETYPE' = 'character',
    'ENUM' = 'factor',
    'DUMMY' = 'character',
    stop('type "', type, '" not implemented')
  )
}
