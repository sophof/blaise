#' @include generics.R
#' @include utils.R
#' @include variable.R

.check_validity_date <- function(object) {
  errors = character()

  if(width(object) != 8L){
    errors = c(errors, paste('DATETYPE only supports width of 8 (YYmmdd)'))
  }

  if(length(errors) == 0) TRUE else errors
}


setClass(
  "variable_date",
  contains = "variable",
  validity = .check_validity_date
)

#====================
# Constructors
setGeneric("variable_date",
           valueClass = 'variable_date',
           function(name, width) standardGeneric("variable_date")
)

setMethod("variable_date",
          signature(
            name = "character",
            width = "numeric"),
          function(name, width)
            new(
              'variable_date',
              name = name,
              type = "DATETYPE",
              width = as.integer(width)
            )
)

setMethod("variable_date",
          signature(
            name = "character",
            width = "missing"),
          function(name)
            variable_date(name, 8L)
)

