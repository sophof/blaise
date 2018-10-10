#' @include generics.R
#' @include utils.R
#' @include variable.R

.check_validity_real <- function(object) {
  errors = character()

  if(!is.na(decimals(object)) & decimals(object) > (width(object) - 2))
    errors = c(errors, 'decimals have to be at least 2 smaller than width')

  if(length(errors) == 0) TRUE else errors
}

setClass(
  "variable_real",
  representation(decimals = "integer"),
  contains = "variable",
  validity = .check_validity_real
)

#====================
# Constructors
.check_size_real = function(object){
  #Check sizes of reals and if they can be accurately represented
  max_digits = .Machine$double.digits
  if (width(object) >= max_digits){
    msg = trimall(sprintf('variable "%s" too wide for double with width "%s",
                          converting to string. max float digits are %i',
                          name(object),
                          width(object),
                          max_digits))
    withRestarts(
      {
        warning(condition(c('real_size_warning', 'warning'), message = msg, call = FALSE))
        object = variable_string(name(object), width(object))
      },
      use_value = function() {}
    )
  }
  return(object)
}


setGeneric("variable_real",
           valueClass = c('variable_real', 'variable_string'),
           function(name, width, decimals) standardGeneric("variable_real")
)

setMethod("variable_real",
          signature(
            name = "character",
            width = "numeric",
            decimals = "missing"),
          function(name, width)
            variable_real(name, width, NA_integer_)
)

setMethod("variable_real",
          signature(
            name = "character",
            width = "numeric",
            decimals = "numeric"),
          function(name, width, decimals) {
            object = new(
              'variable_real',
              name = name,
              type = "REAL",
              width = as.integer(width),
              decimals = as.integer(decimals)
            )
            .check_size_real(object)
          }
)

#====================
# Show method
setMethod('show', 'variable_real', function(object){
  callNextMethod(object)
  cat('\n',
      'decimals: ', decimals(object))
})

#======================
# accessors
setGeneric("decimals",
           valueClass = "integer",
           function(object) standardGeneric("decimals")
)
setMethod("decimals", "variable_real", function(object) object@decimals)
