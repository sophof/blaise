#' @include generics.R
#' @include utils.R

.check_validity_var <- function(object) {
  errors = character()

  if(!object@type %in% .types) {
    errors = c(errors, paste('type', object@type, 'is unknown'))
  }
#
#   if(!is.na(object@decimals) & object@type != 'REAL'){
#     errors = c(errors, paste('only type REAL requires decimals to be known'))
#   }
#
#   if(!is.na(object@decimals) & object@decimals > (object@width - 2)){
#     errors = c(errors, paste('decimals have to be at least 2 smaller than width'))
#
#   if(object@type == 'DATETYPE' & object@width != 8L){
#     errors = c(errors, paste('DATETYPE only supports width of 8 (YYmmdd)'))
#   }
#
#   if(object@type != 'DUMMY' & is.na(object@name)){
#     errors = c(errors, paste('Only DUMMY type can be without a name'))
#   }

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
# Constructors
# .convert_type = function(type) toupper(type)
#
# .check_size = function(object){
#   #Check for sizes of ints and reals and if they can be accurately represented
#   max_int = nchar(.Machine$integer.max)
#   if(object@type == 'INTEGER' & object@width >= max_int){
#     msg = trimall(sprintf('variable "%s" too wide for 32bit int with width "%s",
#                         converting to double. max int is %i',
#                           object@name,
#                           object@width,
#                           .Machine$integer.max))
#     withRestarts(
#       {
#         warning(condition(c('integer_size_warning', 'warning'), message = msg, call = FALSE))
#         object@type = 'REAL'
#       },
#       use_value = function() {}
#     )
#   }
#   #real sizes
#   max_digits = .Machine$double.digits
#   if (object@type == 'REAL' & object@width >= max_digits){
#     msg = trimall(sprintf('variable "%s" too wide for double with width "%s",
#                         converting to string. max float digits are %i',
#                           object@name,
#                           object@width,
#                           max_digits))
#     withRestarts(
#       {
#         warning(condition(c('real_size_warning', 'warning'), message = msg, call = FALSE))
#         object@type = 'STRING'
#       },
#       use_value = function() {}
#     )
#   }
#   return(object)
# }

# setGeneric("variable",
#            valueClass = 'variable',
#            function(name, type, width, object) standardGeneric("variable")
# )
#
# setMethod("variable",
#           signature(
#             name = "character",
#             type = "character",
#             width = "numeric",
#             object = "ANY"),
#           function(name, type, width, object) {
#             name(object) = name
#             type(object) = type
#             width(object) = width
#             return(object)
#           }
# )
#
# setMethod("variable",
#           signature(
#             name = "missing",
#             type = "character",
#             width = "numeric",
#             object = "ANY"),
#           function(type, width, object){
#             name(object) = NA_character_
#             type(object) = type
#             width(object) = width
#             return(object)
#           }
# )

#====================
# Show method
setMethod('show', 'variable', function(object){
  cat(' name    : ', name(object), '\n',
      'type    : ', type(object), '\n',
      'width   : ', width(object), '\n',
      'location: ', location(object))
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

setGeneric("location",
           valueClass = "integer",
           function(object) standardGeneric("location")
)
setMethod("location", "variable", function(object) object@location)
