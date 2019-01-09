#' @include generics.R
#' @include utils.R
#' @include variable.R

.check_validity_string <- function(object) {
  errors = character()

  if((width(object) > 255))
    errors = c(errors, 'width of STRING must be <= 255')

  if(length(errors) == 0) TRUE else errors
}

setClass(
  "variable_string",
  contains = "variable",
  validity = .check_validity_string
)

#====================
# Constructors
setGeneric("variable_string",
           valueClass = 'variable_string',
           function(name, width) standardGeneric("variable_string")
)

setMethod("variable_string",
          signature(
            name = "character",
            width = "numeric"),
          function(name, width) {
            new(
              'variable_string',
              name = name,
              type = "STRING",
              width = as.integer(width)
            )
          }
)
