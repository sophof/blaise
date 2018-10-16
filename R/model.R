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
  print(variables(object))
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
           function(object) standardGeneric("variables")
)

setMethod("variables", "model", function(object) object@variables)

setGeneric("model_names",
           valueClass = "character",
           function(object) standardGeneric("model_names")
)

setMethod("model_names", "model", function(object) sapply(variables(object), name))

setMethod("model_names", "list", function(object) sapply(object, name))

setGeneric("model_types",
           valueClass = "character",
           function(object) standardGeneric("model_types")
)

setMethod("model_types", "model", function(object) sapply(variables(object), type))
setMethod("model_types", "list", function(object) sapply(object, type))

setGeneric("model_widths",
           valueClass = "integer",
           function(object) standardGeneric("model_widths")
)

setMethod("model_widths", "model", function(object) sapply(variables(object), width))
setMethod("model_widths", "list", function(object) sapply(object, width))

setGeneric("model_decimals",
           valueClass = "integer",
           function(object) standardGeneric("model_decimals")
)

setMethod("model_decimals", "model", function(object) {
  vapply(variables(object), decimals, 1L)
}
)

setGeneric("model_labels",
           valueClass = "list",
           function(object, value) standardGeneric("model_labels")
)

setMethod("model_labels", "model", function(object) lapply(variables(object), variable_labels))
setMethod("model_labels", "list", function(object) lapply(object, variable_labels))

setGeneric("model_levels",
           valueClass = "list",
           function(object, value) standardGeneric("model_levels")
)

setMethod("model_levels", "model", function(object) lapply(variables(object), variable_levels))
setMethod("model_levels", "list", function(object) lapply(object, variable_levels))

setGeneric("dummys",
           valueClass = "list",
           function(object) standardGeneric("dummys")
)

setMethod("dummys", "model", function(object) {
  vars = variables(object)
  dummys = sapply(vars, type) == 'DUMMY'
  return(vars[dummys])
}
)

setGeneric("variables_without_dummys",
           valueClass = "list",
           function(object) standardGeneric("variables_without_dummys")
)

setMethod("variables_without_dummys", "model", function(object) {
  vars = variables(object)
  dummys = sapply(vars, is.dummy)
  return(vars[!dummys])
}
)

setGeneric("get_variable",
           valueClass = "variable",
           function(object, name) standardGeneric("get_variable")
)

setMethod("get_variable",
          signature(object = 'model',
                    name = 'character'),
          function(object, name)
            variables(object)[[which(model_names(object) == name)]]
)

setMethod("get_variable",
          signature(object = 'list',
                    name = 'character'),
          function(object, name)
            object[[which(model_names(object) == name)]]
)

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
    namen = sapply(variables(object), name)
    if(is.dummy(variable)){
      dummy_names = stringr::str_extract(namen, '(?<=^DUMMY)\\d+$')
      if(all(is.na(dummy_names)))
        max_i = 0
      else
        max_i = max(as.integer(dummy_names), na.rm = TRUE)
      name(variable) = paste0('DUMMY', max_i + 1)
    }

    if(name(variable) %in% namen)
      stop('duplicate variable names not allowed')

    location(variable) = length(object@variables) + 1
    object@variables = append(object@variables, variable)
    l = length(object@variables)
    names(object@variables)[l] = variable@name
    return(object)
  }
)
