#' @include generics.R
#' @include utils.R
#' @include variable.R

.check_validity_enum <- function(object) {
  errors = character()

  if(any(is.na(variable_labels(object))))
    errors = c(errors, 'type ENUM requires labels to be known')

  if(is.na(object@numbered[1]))
    errors = c(errors, 'type ENUM requires "@numbered" to be known')

  if(any(is.na(variable_levels(object))))
    errors = c(errors, 'NA levels not allowed in enum')

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
           function(name, labels, levels) standardGeneric("variable_enum")
)

setMethod("variable_enum",
          signature(
            name = "character",
            labels = "character",
            levels = "missing"),
          function(name, labels) {
            levels = 1:length(labels)
            new(
              'variable_enum',
              name = name,
              type = "ENUM",
              width = max(nchar(levels)),
              labels = labels,
              levels = levels,
              numbered = FALSE
              )
          }
)

setMethod("variable_enum",
          signature(
            name = "character",
            labels = "character",
            levels = "numeric"),
          function(name, labels, levels) {
            levels = as.integer(levels)
            num = !all(levels == 1:length(levels))
            new(
              'variable_enum',
              name = name,
              type = "ENUM",
              width = max(nchar(levels)),
              labels = labels,
              levels = levels,
              numbered = num
            )
          }
)

setMethod("variable_enum",
          signature(
            name = "character",
            labels = "character",
            levels = "character"),
          function(name, labels, levels)
            variable_enum(name, labels = labels, levels = as.integer(levels))
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
