#' @include generics.R

.check_validity <- function(object) {
  errors = character()

  if(!object@type %in% .types) {
    errors = c(errors, paste('type', object@type, 'is unknown'))
  }

  if(!is.na(object@decimals) & object@type != 'REAL'){
    errors = c(errors, paste('only type REAL requires decimals to be known'))
  }

  if(!is.na(object@decimals) & object@decimals > (object@width - 2)){
    errors = c(errors, paste('decimals have to be at least 2 smaller than width'))
  }

  if(is.na(object@labels[1]) & object@type == 'ENUM'){
    errors = c(errors, paste('type ENUM requires labels to be known'))
  }

  if(!is.na(object@labels[1]) & object@type != 'ENUM'){
    errors = c(errors, paste('only type ENUM requires labels to be known'))
  }

  if(object@type == 'DATETYPE' & object@width != 8L){
    errors = c(errors, paste('DATETYPE only supports width of 8 (YYmmdd)'))
  }

  if(object@type != 'DUMMY' & is.na(object@name)){
    errors = c(errors, paste('Only DUMMY type can be without a name'))
  }

  if(length(errors) == 0) TRUE else errors
}

setClass(
  "variable",
  representation(
    name = "character",
    type = "character",
    width = "integer",
    decimals = "integer",
    labels = "character"),
  validity = .check_validity
)

#====================
# Constructors
.convert_type = function(type) toupper(type)

setGeneric("variable",
           valueClass = 'variable',
           function(name, type, width, decimals, labels) standardGeneric("variable")
)

setMethod("variable",
          signature(
            name = "character",
            type = "character",
            width = "numeric",
            decimals = 'missingOrNULL',
            labels = 'missingOrNULL'),
          function(name, type, width, decimals, labels) {
            object = new(
              'variable',
              name = name,
              type = .convert_type(type),
              width = as.integer(width),
              decimals = NA_integer_,
              labels = NA_character_)
          }
)

setMethod("variable",
          signature(
            name = "character",
            type = "character",
            width = "numeric",
            decimals = 'numeric',
            labels = 'missingOrNULL'),
          function(name, type, width, decimals, labels) new(
            'variable',
            name = name,
            type = .convert_type(type),
            width = as.integer(width),
            decimals = if_else(as.integer(decimals) == 0, NA_integer_, as.integer(decimals)),
            labels = NA_character_)
)

setMethod("variable",
          signature(
            name = "character",
            type = "character",
            width = "numeric",
            decimals = 'missingOrNULL',
            labels = 'character'),
          function(name, type, width, decimals, labels) new(
            'variable',
            name = name,
            type = .convert_type(type),
            width = as.integer(width),
            decimals = NA_integer_,
            labels = labels)
)

setMethod("variable",
          signature(
            name = "character",
            type = "character",
            width = "numeric",
            decimals = 'numeric',
            labels = 'character'),
          function(name, type, width, decimals, labels) new(
            'variable',
            name = name,
            type = .convert_type(type),
            width = as.integer(width),
            decimals = as.integer(decimals),
            labels = labels)
)

setMethod("variable",
          signature(
            name = "missing",
            type = "character",
            width = "numeric",
            decimals = 'missing',
            labels = 'missing'),
          function(type, width) new(
            'variable',
            name = NA_character_,
            type = .convert_type(type),
            width = as.integer(width),
            decimals = NA_integer_,
            labels = NA_character_)
)

#====================
# Show method
setMethod('show', 'variable', function(object){
  cat(' name    : ', object@name, '\n',
      'type    : ', object@type, '\n',
      'width   : ', object@width, '\n',
      'decimals: ', object@decimals, '\n',
      'labels  : ', object@labels)
})


#======================
# accessors

setMethod("name<-", "variable", function(object, value){
  object@name = value
  object
})

setMethod("name", "variable", function(object) object@name)

setMethod("type", "variable", function(object) object@type)
