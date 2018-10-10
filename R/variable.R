#' @include generics.R
#' @include utils.R

.check_validity_var <- function(object) {
  errors = character()

  if(!object@type %in% .types) {
    errors = c(errors, paste('type', object@type, 'is unknown'))
  }

  if(length(errors) == 0) TRUE else errors
}

setClass(
  "variable",
  representation(
    "VIRTUAL",
    name = "character",
    type = "character",
    width = "integer",
    location = 'integer'),
  validity = .check_validity_var
)

#====================
# Show method
setMethod('show', 'variable', function(object){
  cat(' name    : ', name(object), '\n',
      'type    : ', type(object), '\n',
      'width   : ', width(object), '\n',
      'location: ', get_location(object))
})


#======================
# accessors

setMethod("name<-", "variable", function(object, value){
  object@name = value
  object
})

setMethod("name", "variable", function(object) object@name)

setMethod("type", "variable", function(object) object@type)

setMethod("width", "variable", function(object) object@width)

setGeneric("location<-",
           valueClass = "variable",
           function(object, value) standardGeneric("location<-")
)

setMethod("location<-", "variable", function(object, value){
  object@location = as.integer(value)
  object
})

setGeneric("get_location",
           valueClass = "integer",
           function(object) standardGeneric("get_location")
)
setMethod("get_location", "variable", function(object) object@location)

setGeneric("variable_levels",
           valueClass = "integer",
           function(object) standardGeneric("variable_levels")
)
setMethod("variable_levels", "variable", function(object) NA_integer_)

setGeneric("variable_labels",
           valueClass = "character",
           function(object) standardGeneric("variable_labels")
)
setMethod("variable_labels", "variable", function(object) NA_character_)

