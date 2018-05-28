#' @include variable.R generics.R
NULL

setClass(
  "model",
  representation(
    name = "character",
    variables = "list"),
  prototype(
    name = NA_character_)
)

#====================
# Constructors
setGeneric("model",
           valueClass = 'model',
           function(name, variables, ...) standardGeneric("model")
)

setMethod("model", signature(name="missing", variables="missing"),
          function(...) new('model')
)

setMethod("model", signature(name="character", variables="missing"),
          function(name, ...) new('model', name = name)
)

setMethod("model", signature(name='character', variables="variable"),
          function(name, variables, ...) {
            m = new('model', name = name)
            m = addVariable(m, variables)
            return(m)
            })

setMethod("model", signature(name='character', variables="list"),
          function(name, variables, ...) {
            m = new('model', name = name)
            if(!all(sapply(variables, function(x) is(x, 'variable')))){
              stop('list must contain only variables of class variable')
            }

            for (v in variables) {
              m = addVariable(m, v)
            }
            return(m)
          })

#====================
# Show method
setMethod('show', 'model', function(object){
  cat('blaise datamodel: ', object@name, '\n\n')
  print(object@variables)
})


#======================
# accessors for name
setMethod("name<-", "model", function(object, value){
  object@name = value
  object
})

setMethod("name", "model", function(object) object@name)

#======================
# accessors for variables
setGeneric("variables",
           valueClass = "list",
           function(object, value) standardGeneric("variables")
)

setMethod("variables", "model", function(object) object@variables)

setGeneric("variable_names",
           valueClass = "character",
           function(object, value) standardGeneric("variable_names")
)

setMethod("variable_names", "model", function(object) sapply(object@variables, name))

setGeneric("variable_types",
           valueClass = "character",
           function(object, value) standardGeneric("variable_types")
)

setMethod("variable_types", "model", function(object) sapply(object@variables, type))

setGeneric("variable_widths",
           valueClass = "integer",
           function(object, value) standardGeneric("variable_widths")
)

setMethod("variable_widths", "model", function(object) sapply(object@variables, function(v) v@width))

setGeneric("variable_decimals",
           valueClass = "integer",
           function(object, value) standardGeneric("variable_decimals")
)

setMethod("variable_decimals", "model", function(object) sapply(object@variables, function(v) v@decimals))

setGeneric("variable_labels",
           valueClass = "list",
           function(object, value) standardGeneric("variable_labels")
)

setMethod("variable_labels", "model", function(object) lapply(object@variables, function(v) v@labels))

#=====================
# add variables to model
setGeneric("addVariable",
           valueClass = 'model',
           function(object, variable)
             standardGeneric('addVariable'))

setMethod(
  'addVariable',
  signature(
    object = 'model',
    variable = 'variable'),
  function(object, variable) {
    namen = names(object@variables)
    if(name(variable) %in% namen & !is.na(name(variable)))
      stop('duplicate variable names not allowed')
    object@variables = append(object@variables, variable)
    l = length(object@variables)
    names(object@variables)[l] = variable@name
    return(object)
  }
)
