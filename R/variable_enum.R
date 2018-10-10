#' @include generics.R
#' @include utils.R
#' @include variable.R

.check_validity_enum <- function(object) {
  errors = character()

  if(is.na(object@labels[1]))
    errors = c(errors, 'type ENUM requires labels to be known')

  if(is.na(object@numbered[1]))
    errors = c(errors, 'type ENUM requires "@numbered" to be known')

  if(length(errors) == 0) TRUE else errors
}

setClass(
  "variable_enum",
  representation(
    levels = "integer",
    labels = "character",
    numbered = "logical"),
  contains = "variable",
  validity = .check_validity_enum
)

#====================
# Constructors
setGeneric("variable_enum",
           valueClass = 'variable_enum',
           function(name, width, labels, levels) standardGeneric("variable_enum")
)

setMethod("variable_enum",
          signature(
            name = "character",
            width = "numeric",
            labels = "character",
            levels = "missing"),
          function(name, width, labels) {
            new(
              'variable_enum',
              name = name,
              type = "ENUM",
              width = as.integer(width),
              labels = labels,
              levels = 1:length(labels),
              numbered = FALSE
              )
          }
)

setMethod("variable_enum",
          signature(
            name = "character",
            width = "numeric",
            labels = "character",
            levels = "numeric"),
          function(name, width, labels, levels) {
            levels = as.integer(levels)
            num = !all(levels == 1:length(levels))
            new(
              'variable_enum',
              name = name,
              type = "ENUM",
              width = as.integer(width),
              labels = labels,
              levels = levels,
              numbered = num
            )
          }
)

setMethod("variable_enum",
          signature(
            name = "character",
            width = "numeric",
            labels = "character",
            levels = "character"),
          function(name, width, labels, levels)
            variable_enum(name, width, labels = labels, levels = as.integer(levels))
)

#====================
# Show method
setMethod('show', 'variable_enum', function(object){
  callNextMethod(object)
  cat('\n',
      'labels  : ', paste(sprintf("(%i:'%s')", variable_levels(object), variable_labels(object)), collapse = '|') , '\n',
      'numbered: ', is.numbered_enum(object))
})

#======================
# accessors

setMethod("variable_levels", "variable_enum", function(object) object@levels)
setMethod("variable_labels", "variable_enum", function(object) object@labels)

#======================
# Checks
setGeneric("is.numbered_enum",
           valueClass = "logical",
           function(object) standardGeneric("is.numbered_enum")
)
setMethod("is.numbered_enum", "variable_enum",
          function(object)
            object@numbered
)
# This will only be called if the variable is not of type variable_enum
setMethod("is.numbered_enum", "variable",
          function(object)
            FALSE
)
setMethod("is.numbered_enum", "character",
          function(object)
            all(stringr::str_detect(object, '^\\d+$'))
)
