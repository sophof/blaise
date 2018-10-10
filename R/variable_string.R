#' @include generics.R
#' @include utils.R
#' @include variable.R

setClass(
  "variable_string",
  contains = "variable"
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
