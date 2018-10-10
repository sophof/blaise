#' @include generics.R
#' @include utils.R
#' @include variable.R

setClass(
  "variable_integer",
  contains = "variable"
)

#====================
# Constructors
.check_size_integer = function(object){
  # Check size of ints and if they can be accurately represented
  max_int = nchar(.Machine$integer.max)
  if(width(object) >= max_int){
    msg = trimall(sprintf('variable "%s" too wide for 32bit int with width "%s",
                          converting to double. max int is %i',
                          name(object),
                          width(object),
                          .Machine$integer.max))
    withRestarts(
      {
        warning(condition(c('integer_size_warning', 'warning'), message = msg, call = sys.call(-1)))
        object = variable_real(name(object), width(object))
      },
      use_value = function() {}
    )
  }
  return(object)
}


setGeneric("variable_integer",
           valueClass = c('variable_integer', 'variable_real'),
           function(name, width) standardGeneric("variable_integer")
)

setMethod("variable_integer",
          signature(
            name = "character",
            width = "numeric"),
          function(name, width) {
            object = new(
              'variable_integer',
              name = name,
              type = "INTEGER",
              width = as.integer(width)
            )
            .check_size_integer(object)
          }
)

