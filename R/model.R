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
           function(name, variable_names, ...) standardGeneric("model")
)

setMethod("model", signature(name="missing", variable_names="missing"),
          function(...) new('model')
)

setMethod("model", signature(name="character", variable_names="missing"),
          function(name, ...) new('model', name = name)
)

#====================
# Show method
setMethod('show', 'model', function(object){
  cat('blaise datamodel: ', object@name, '\n\n')
  print(object@variables)
})


#======================
# accessors for name
setGeneric("name<-",
           valueClass = "model",
           function(object, value) standardGeneric("name<-")
)

setMethod("name<-", "model", function(object, value){
  object@name = value
  object
})

setGeneric("name",
           valueClass = "character",
           function(object) standardGeneric("name")
)

setMethod("name", "model", function(object) object@name)

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
    object@variables = append(object@variables, variable)
    l = length(object@variables)
    names(object@variables)[l] = variable@name
    return(object)
  }
)
