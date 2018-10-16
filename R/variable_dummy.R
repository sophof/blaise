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

#======================
# Checks
setGeneric("is.dummy",
           valueClass = "logical",
           function(object) standardGeneric("is.dummy")
)
setMethod("is.dummy", "variable_dummy",
          function(object)
            TRUE
)
# This will only be called if the variable is not of type variable_dummy
setMethod("is.dummy", "variable",
          function(object)
            FALSE
)
