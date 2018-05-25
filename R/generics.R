setClassUnion("missingOrNULL", c("missing", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))

setGeneric("name<-",
           valueClass = "variable",
           function(object, value) standardGeneric("name<-")
)

setGeneric("name",
           valueClass = "character",
           function(object) standardGeneric("name")
)

setGeneric("type",
           valueClass = "character",
           function(object) standardGeneric("type")
)

.types = c(
  'STRING',
  'INTEGER',
  'REAL',
  'DATETYPE',
  'ENUM',
  'DUMMY'
)
