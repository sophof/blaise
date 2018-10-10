#' @include generics.R
#' @include utils.R
#' @include variable.R

setClass(
  "variable_dummy",
  contains = "variable"
)

#====================
# Constructors
setGeneric("variable_dummy",
           valueClass = 'variable_dummy',
           function(width) standardGeneric("variable_dummy")
)

setMethod("variable_dummy",
          signature(width = "numeric"),
          function(width)
            new(
              'variable_dummy',
              name = NA_character_,
              type = "DUMMY",
              width = as.integer(width)
            )
)

