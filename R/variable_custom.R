#' @include generics.R
#' @include utils.R
#' @include variable.R

.check_validity_custom <- function(object) {
  errors = character()

  if(is.na(object@labels[1]))
    errors = c(errors, 'type CUSTOM requires labels to be known')

  if(is.na(object@levels[1]))
    errors = c(errors, 'type CUSTOM requires levels to be known')

  if(length(errors) == 0) TRUE else errors
}

setClass(
  "variable_custom",
  representation(
    levels = "integer",
    labels = "character"),
  contains = "variable",
  validity = .check_validity_custom
)

#====================
# Constructors
setGeneric("variable_custom",
           valueClass = 'variable_custom',
           function(name, labels, levels) standardGeneric("variable_custom")
)

setMethod("variable_custom",
          signature(
            name = "character",
            labels = "character",
            levels = "numeric"),
          function(name, labels, levels)
            new(
              'variable_custom',
              name = name,
              type = "ENUM",
              width = max(nchar(as.integer(levels))),
              labels = labels,
              levels = as.integer(levels)
            )
)

setMethod("variable_custom",
          signature(
            name = "character",
            labels = "character",
            levels = "character"),
          function(name, labels, levels)
            variable_custom(name, labels = labels, levels = as.integer(levels))
)

# Use this to build an enum variable from a custom type
setGeneric("variable_from_custom",
           valueClass = 'variable_enum',
           function(name, custom_type) standardGeneric("variable_from_custom")
)
setMethod("variable_from_custom",
          signature(
            name = "character",
            custom_type = "variable_custom"
          ),
          function(name, custom_type)
            variable_enum(
              name = name,
              labels = as.character(variable_levels(custom_type)),
              levels = variable_levels(custom_type)
            )
)

#====================
# Show method
setMethod('show', 'variable_custom', function(object){
  callNextMethod(object)
  cat('\n',
      'labels  : ', paste(sprintf("(%i:'%s')", variable_levels(object), variable_labels(object)), collapse = '|')
  )
})

#======================
# accessors
setMethod("variable_levels", "variable_custom", function(object) object@levels)
setMethod("variable_labels", "variable_custom", function(object) object@labels)
