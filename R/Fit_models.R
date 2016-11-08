#' Create a new model
#'
#' This function creates a model to be fitted.
#' @param type A function which defines the curve to fit (\code{\link[NMR.Utils]{pseudoVoigt}} is the only predefined model function)
#' @param ... Initial guesses for the parameters of the function
#' @return A model object
#' @export
#' @examples
#' new_model(pseudoVoigt, height=1e9,centre=-9.95,hwhm=2.1,shape=1)
new_model <- function(type, ...) {
  return(list(model=type,constraint_type=list(),constraint=list(), estimation_function=NA,initial_guess=list(...),current_guess=NA,upper=list(),lower=list()))
}

#' Add a constraint to a model
#'
#' This function adds a constraint to a model. Models can only have one constraint per parameter. Running this function multiple times with the same model and parameter will replace the existing constraint.
#' @param model The model object to add the constraint to
#' @param parameter The name of the parameter for which the constraint will be added
#' @param constraint_type The type of constraint to add, one of ("range", "vary", "variable_range") – see details
#' @param constraint Either c(min, max) value or c(value, min, max) depending on constraint_type – see details
#' @return A model object
#' @export
#' @section Details:
#' There are 3 types of constraint currently implemented:
#' \itemize{
#' \item range: Allow the value of this parameter to vary between the 2 values given.
#' \item vary: Allow the value of this parameter to vary by this ammount since the previous scan
#' \item variable_range: Allow the value of this parameter to vary by this ammount since the previous scan, provided it remains within the range given.
#' }
#' Values of the constraint parameter are thus:
#' \itemize{
#' \item range: c(min, max)
#' \item vary: value
#' \item variable_range: c(value, min, max)
#' }
#' @examples
#' add_constraint_to_model(model, "height", "range", c(0,1))
#' add_constraint_to_model(model, "height", "vary", 0.5)
#' add_constraint_to_model(model, "height", "variable_range", c(0.5,0,1))
add_constraint_to_model <- function(model,parameter, constraint_type,constraint) {
  constraint_types=c("range","vary","variable_range")
  
  if(!constraint_type %in% constraint_types) {
    stop(paste0('Constraint type "',constraint_type, '" unknown.'))
  }
  if(!all(is.numeric(constraint))) {
    stop('Constraint values must be numeric')
  }
  
  if(constraint_type=='range') {
    if(!length(constraint)==2) {
      stop('Range constraint takes 2 values')
    }
  } else if(constraint_type=='vary') {
    if(!length(constraint)==1) {
      stop('Range constraint takes 1 value')
    }
  } else if(constraint_type=='variable_range') {
    if(!length(constraint)==3) {
      stop('Variable range constraint takes 3 values')
    }
  }
  
  model$constraint_type[parameter]=constraint_type
  model$constraint[parameter]=list(constraint)
  return(model)
}

#' Add an estimation function to a model
#'
#' This function adds an estimation function to a model. 
#' @param model The model object to which the estimation function should be added (see details)
#' @return The model object
#' @section Details:
#' The model function recieves the scan number, initial guess and xy data.
#' It should return a list of parameter estimates (can be a sparse list)
#' @export
#' @examples
#' #estimates the height of a peak based on the intensity at its (guessed) centre
#' guess_height <- function(n,guess,xy) {
#'  guess_height=rev(xy[[2]])[findInterval(guess$centre,rev(xy[[1]]))]
#'  list(height=guess_height)
#' }
#' add_estimation_function_to_model(model, guess_height)
add_estimation_function_to_model <- function(model, estimation_function) {
  if(!is.function(estimation_function)) {
    stop('The estimation function must be a function.')
  }
  model$estimation_function=estimation_function
  return(model)
}
