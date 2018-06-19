#' Create a new fit
#'
#' This function creates a fit that can contain models.
#' @param integration_range The range over which to perform the integration c(min,max)
#' @param noise_height Approximate magnitude of the noise in the spectrum (probably best as 0)
#' @param maxEvaluations Max number of calls to model function
#' @param maxIterations Max number of iterations during fitting
#' @param use_previous_as_guess Uses the previous fit as a guess for the next fit (rather than initial guess)
#' @param brute_force Attempt to brute force the starting parameters
#' @param brute_force_method 'short' or 'full'. Short only tries to fit using all of the models, full will also try every combination of models.
#' @param brute_force_grid_size Number of parameters to try for every constraint during the brute force.
#' @return A fit object
#' @export
#' @examples
#' new_fit()
new_fit <- function(integration_range=c(-Inf,Inf),noise_height=0,maxEvaluations=10000,maxIterations=1000,use_previous_as_guess=TRUE, brute_force=FALSE,brute_force_method='short',brute_force_grid_size=10) {
  fit = list(models=list(),integration_range=integration_range,noise_height=noise_height, maxEvaluations=maxEvaluations, maxIterations=maxIterations, determine_parameters_dynamically=FALSE, determine_parameters_from_scans=NA, use_previous_as_guess=use_previous_as_guess,brute_force=brute_force, brute_force_method=brute_force_method,brute_force_grid_size=brute_force_grid_size,update_guess_for_non_fixed=TRUE,ppm_exclude=list(),result=NA,last_scan=0, timeAxis=NA)
  class(fit) <- c('nmr.fit.object', 'list')
  fit
}

#' Export fit results
#'
#' This function is used to export the fit results as a csv file.
#' @param fit The fit object
#' @param filename The path to the file (ending in .csv)
#' @return None
#' @export
#' @examples
#' export(fit,"/path/to/file.csv")
export.nmr.fit.object <- function(fit, filename) {
  write.table(fit$result,filename,sep=",",row.names =FALSE)
}


#' Make dynamic fit
#'
#' This function makes the fit run as a dynamic fit. For this to work effectively, there must be one scan per model peak that contains ONLY this peak in the region of interest (no overlap). Otherwise the model will have to be fully defined manually.
#' @param fit The fit object to make dynamic
#' @param scans Scans containing only a single peak in the region of interest (one per model peak)
#' @param update_guess_for_non_fixed Use the automatically determined parameters for non-fixed parameters (as well as for the fixed ones)
#' @return The fit object
#' @export
make_dynamic_fit <-function(fit,scans, update_guess_for_non_fixed=TRUE) {
  fit$determine_parameters_dynamically=TRUE
  fit$determine_parameters_from_scans=scans
  fit$update_guess_for_non_fixed=update_guess_for_non_fixed
  return(fit)
}

#' Exclude a ppm range from the fit
#'
#' This function excludes a ppm range from the fit.
#' @param fit The fit object from which to exclude the ppm range.
#' @param range The range to exclude c(start,end) -- start < end
#' @return The fit object
#' @export
exclude_ppm_range_from_fit <-function(fit,range) {
  fit$ppm_exclude=append(fit$ppm_exclude, range)
  return(fit)
}

#' Add a model to a fit
#'
#' This function adds a model object to a fit. The model must have all constraints etc. added prior to adding it to a fit.
#' @param fit The fit object to add the model to
#' @param model The model object to add
#' @return A fit object
#' @export
#' @examples
#' add_model_to_fit(fit,model)
add_model_to_fit <- function(fit,model) {
  if(is.na(model$name))
    model$name = deparse(substitute(model))
  fit$models=append(fit$models,list(model))
  return(fit)
}

#' Fixes the uncronstrained parameters for every model
#'
#' This function modifies the function for each model, adding default values for the parameters not to be fitted.
#' @param fit The fit object to use
#' @return The fit object
#' @examples
#' fix_unconstrained_parameters(fit)
#' @keywords internal
fix_unconstrained_parameters <-function(fit) {
  lenm=length(fit$models)
  for(m in 1:lenm) {
    model=fit$models[[m]]
    fmls=names(formals(model$model))
    fixed=fmls[!fmls %in% names(model$constraint)]
    fixed=fixed[!fixed=='x']
    formals(model$model)[fixed]=model$initial_guess[fixed]
    model$current_guess=model$initial_guess[names(model$constraint)]
    fit$models[[m]]=model
  }
  return(fit)
}

#' Generares the result data frame
#'
#' This function creates an empty data frame to hold the fitting resutls
#' @param fit The fit object to use
#' @param n The number of rows to add to the data frame
#' @return The fit object
#' @examples
#' prepare_result_data_frame(fit,n)
#' @keywords internal
prepare_result_data_frame <-function(fit,n) {
  columns=c("std.error",'fit_integrated_area')
  model_extra_columns=c('std.error','integrated_area')
  lenm=length(fit$models)
  for(m in 1:lenm) {
    model=fit$models[[m]]
    columns=append(columns, paste0(names(model$constraint),'_',m))
    columns=append(columns, paste0(model_extra_columns,'_',m))
  }
  fit$result=data.frame(rep_len(list(rep_len(NA,n)),length(columns)))
  names(fit$result)<-columns
  return(fit)
}

#'Generate fit function
#'
#' This function creates a function representing the overall fit
#' @param models List of models to use
#' @return The fit function
#' @examples
#' func=get_fit_function(models)
#' @keywords internal
get_fit_function <- function(models) {
  if(length(models)>0) {
    total_fit_function=function(x) rowSums(sapply(models,function(y) y[[2]](x)))
  } else {
    total_fit_function=function(x) rep_len(0,length(x))
  }
  return(total_fit_function)
}

#' Generate a list of model functions that only take an x parameter
#'
#' This function converts a list of models to a list of functions + model indexes.
#' @param models The models list
#' @param result_line A row from the results table
#' @return list(c(model_index, fitted_model),...)
#' @examples
#' make_fitted_models(models,result_line)
#' @keywords internal
make_fitted_models <-function(models,result_line) {
  nmodels=length(models)
  total_fit=c()
  for(m in 1:nmodels) {
    model=models[[m]]

    #Extract fitted values
    variables=names(model$constraint)
    column_names=paste0(variables,"_",m)
    values=as.list(result_line[,column_names])
    names(values)<-variables
    if(any(is.na(values))) {
      next
    } else {

      #Define fitted function
      model_fitted=model$model

      #Assign fitted values to it
      fmls=names(formals(model_fitted))
      fixed=fmls[fmls %in% variables]
      formals(model_fitted)[fixed]=values[fixed]

      #Add function to overall fit
      total_fit=append(total_fit,list(c(m,model_fitted)))
    }
  }
    return(total_fit)
}
