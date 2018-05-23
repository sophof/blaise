#' @include generics.R

.check_validity <- function(object) {
  errors = character()

  types = c(
    'STRING',
    'INTEGER',
    'REAL',
    'DATETYPE',
    'ENUM'
  )
  if(!object@type %in% types) {
    errors = c(errors, paste('type', object@type, 'is unknown'))
  }

  if(!is.na(object@decimals) & object@type != 'REAL'){
    errors = c(errors, paste('only type REAL requires decimals to be known'))
  }

  if(!is.na(object@decimals) & object@decimals > (object@width - 1)){
    errors = c(errors, paste('only type REAL requires decimals to be known'))
  }

  if(is.na(object@labels[1]) & object@type == 'ENUM'){
    errors = c(errors, paste('type ENUM requires labels to be known'))
  }

  if(!is.na(object@labels[1]) & object@type != 'ENUM'){
    errors = c(errors, paste('only type ENUM requires labels to be known'))
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
            .init(object)
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
            decimals = as.integer(decimals),
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
