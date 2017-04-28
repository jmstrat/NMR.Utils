#' Run a fit
#'
#' This function fits the data using the models defined previously.
#' @param fit The fit object to use
#' @param data The data to fit
#' @return The fit object
#' @export
#' @examples
#' run_fit_for_data(fit, data)
run_fit_for_data <-function(fit,data) {
  load_or_install("minpack.lm")

	#Number of scans to fit = number of columns - 1 (first col == x values)
	nc=ncol(data)

	if(fit$determine_parameters_dynamically) {
    warning("Automatic parameter calculation has not been re-written for this library yet... (& it may never be)")
		#TODO:
    #run function to determine parameters here...
	}

  fit=fix_unconstrained_parameters(fit)
  fit=prepare_result_data_frame(fit,nc-1)

  progress=time_remaining_start(nc-1)
  #Begin loop over scans
  for(i in 2:nc) {
    fit=fit_single_scan(fit, data[,c(1,i)])
    fit=integrate_last_scan(fit,data[,c(1,i)])

    fit$last_scan=i-1
    progress(i-1)
  }
  return(fit)
}

#' Run a fit on a single scan
#'
#' This function fits a single scan using the models defined previously.
#' @param fit The fit object to use
#' @param data The data to fit
#' @return The fit object
#' @examples
#' fit_single_scan(fit, data)
#' @keywords internal
fit_single_scan <- function(fit, data) {
  n_models=length(fit$models)
  x=data[,1]
  y=data[,2]

  #ignore data outside the integration range
  y[x>fit$integration_range[[2]]]<-NA
  y[x<fit$integration_range[[1]]]<-NA
  #ignore any regions specified by the user
  for(region in fit$ppm_exclude) {
    y[x<region[[2]]&x>region[[1]]]<-NA
  }

  sn=fit$last_scan
  nModel=length(fit$models)

  #Update models
  for(m in 1:nModel) {
    model=fit$models[[m]]

    #Get previous values for model parameters
    if(sn>0) {
      vars=names(model$current_guess)
      previous_values=as.list(fit$result[sn,paste0(vars,"_",m)])
      names(previous_values)=vars
      nanames=vars[is.na(previous_values)]
      previous_values[nanames]=model$current_guess[nanames]
    } else {
      #No previous values: use guess
      previous_values=model$current_guess
    }
    if(fit$use_previous_as_guess) {
      #We need to update the current guess to the values from the last fit
      model$current_guess=previous_values
    }
    if(is.function(model$estimation_function)) {
      #Call the estimation function to update values
      estimate=model$estimation_function(i,model_list[[m]]$guess,c(x,y))
      estimate=estimate[is.numeric(unlist(estimate))]
      vars=names(estimate)
      model$current_guess[vars]=estimate
    }
    #Process constraints:
    #Process range constraints
    range_constraints=names(model$constraint[model$constraint_type=='range'])
    model$upper[range_constraints]=unlist(sapply(model$constraint[range_constraints], "[[", 2))
    model$lower[range_constraints]=unlist(sapply(model$constraint[range_constraints], "[[", 1))
    #Process vary constraints
    vary_constraints=names(model$constraint[model$constraint_type=='vary'])
    model$upper[vary_constraints]=unlist(previous_values[vary_constraints]) + unlist(model$constraint[vary_constraints])
    model$lower[vary_constraints]=unlist(previous_values[vary_constraints]) - unlist(model$constraint[vary_constraints])
    #Process variable range constraints
    vr_constraints=names(model$constraint[model$constraint_type=='variable_range'])
    #set upper and lower in same manner as for vary constraints
    model$upper[vr_constraints]=unlist(previous_values[vr_constraints])+unlist(sapply(model$constraint[vr_constraints], "[[", 1))
    model$lower[vr_constraints]=unlist(previous_values[vr_constraints])-unlist(sapply(model$constraint[vr_constraints], "[[", 1))
    #Put any values outside the limits into the limits
    upper_lim=names(model$constraint[vr_constraints][model$upper[vr_constraints]>sapply(model$constraint[vr_constraints], "[[", 3)])
    model$upper[upper_lim]<-unlist(sapply(model$constraint[upper_lim], "[[", 3))
    lower_lim=names(model$constraint[vr_constraints][model$lower[vr_constraints]<sapply(model$constraint[vr_constraints], "[[", 2)])
    model$lower[lower_lim]<-unlist(sapply(model$constraint[lower_lim], "[[", 2))

    #Fix any ordering issues:
    model$lower=model$lower[names(model$constraint)]
    model$upper=model$upper[names(model$constraint)]
    #Update model
    fit$models[[m]]=model
  }
  #Determine control values
  control=nls.lm.control(maxiter=fit$maxIterations,epsfcn=fit$noise_height,maxfev=fit$maxEvaluations)

  #For saving results:
  best_r2=Inf
  best_fit=NA
  best_models=NA
  if(fit$brute_force & fit$brute_force_method=='short') {
    #Run the fit
    best_fit=fit_models_to_x_y(fit$models,x,y, control,TRUE, fit$brute_force_grid_size)
    best_models=c(1:n_models)
  } else {
    #We attempt every combination of models to ensure we have the best fit
    for(comb_len in 1:n_models) {
      cmb=combn(1:n_models,comb_len)
      for(comb in 1:ncol(cmb)) {
        current_combination=cmb[,comb]
        current_models=fit$models[current_combination]
        if(fit$brute_force) {
          fit_result=fit_models_to_x_y(current_models,x,y, control,TRUE, fit$brute_force_grid_size)
        } else {
          #Run the fit
          fit_result=fit_models_to_x_y(current_models,x,y, control)
        }

        #Check if it failled
        if(all(is.na(fit_result)))
          next

        #If it didn't fail, calculate r^2 and compare to previous attempts
        r2=sqrt(sum(residuals(fit_result)^2))
        if(r2 < best_r2) {
          best_r2=r2
          best_fit=fit_result
          best_models=current_combination
        }
      }
    }
  }

  if(all(is.na(best_fit))){
    #All fits failled -- print warning and return
    warning(paste0("[",sn+1,"] Could not fit data."), call. = FALSE)
    #Give up
    return(fit)
  }

  #Extract parameters:
  fitted_parameters=coef(best_fit)
  #BEWARE: model numbers from fit result will NOT be the same as in the fit object (must convert using best_models)

  #Convert unique variables from fit back to model variables and store in results table
  fitted_names=strsplit(names(fitted_parameters),'_')
  for(p in 1:length(fitted_names)) {
    param=fitted_names[[p]]
    var=param[[1]]
    #Convert between model numbers
    model=best_models[[as.numeric(param[[2]])]]
    #Get value
    value=as.numeric(fitted_parameters[[p]])
    #Append data to results table
    column_name=paste0(var,"_",model)
    fit$result[sn+1,column_name]=value
  }

  #std. error = square root of r^2 divided by number of degrees of freedom (I think???)
  sde=sqrt(sum(residuals(best_fit)^2)/summary(best_fit)$df[2])

  #Add to results table
  fit$result[sn+1,"std.error"]=sde

  return(fit)
}

#' Run a single least-squares fit
#'
#' This function fits a single scan using the models provided.
#' @param models The models to use
#' @param x The x data to fit
#' @param y The y data to fit
#' @param control The nls control parameters
#' @param brute_force Should brute force the starting parameters
#' @param brute_force_grid_size Number of parameters to try for every constraint during the brute force.
#' @return The least-squares result
#' @examples
#' fit_models_to_x_y(models, x, y)
#' @keywords internal
fit_models_to_x_y <- function(models, x, y, control,brute_force=FALSE,brute_force_grid_size=1) {
  formula="y~"
  start=list()
  upper=c()
  lower=c()

  #Define tolerances to be v.low (not sure whether this helps or not)
  control$ftol=1e-10
  control$ptol=1e-10
  control$etol=1e-10

  nModel=length(models)
  for(m in 1:nModel) {
    model=models[[m]]
    #Generate fit formula BEWARE: model numbers here will NOT be the same as in the fit object
    variables=names(model$constraint)
    formula=paste(formula,paste0("models[[",m,"]]$model(x,",paste(variables,variables,sep='=',collapse=paste0('_',m,',')),'_',m,")"),sep='+')
    #Determine upper and lower values based on constraints
    upper[paste0(names(model$upper),'_',m)]=c(unlist(model$upper))
    lower[paste0(names(model$lower),'_',m)]=c(unlist(model$lower))
    #Determine current guess
    start[paste0(names(model$current_guess),'_',m)]=c(unlist(model$current_guess))
  }
  formula=eval(parse(text=formula))

  if(brute_force) {
    bf_res=fit_initial_brute_force(models,x,y,formula,upper,lower,grid_points=brute_force_grid_size)
    if(!any(is.na(bf_res))) {
      start=bf_res
    }
  }

  #Run the fit
  return(tryCatch({nlsLM(formula, start=start, lower=lower, upper=upper, control = control)}, error = function(e) {
    #warning(e)
    return(NA)}))
}

#' Brute force the starting parameter
#'
#' This function runs many fits to sample the landscape for local minima.
#' @param models The models to use
#' @param x The x data to fit
#' @param y The y data to fit
#' @param formula The nls formula
#' @param upper The upper bounds
#' @param lower The lower bounds
#' @param grid_points The number of points in the grid to sample
#' @return The best coefficients
#' @examples
#' fit_initial_brute_force(models,x,y,formula,upper,lower)
#' @keywords internal
fit_initial_brute_force <-function(models,x,y,formula,upper,lower,grid_points=100) {
  load_or_install('nls2')
  ## Define the grid to search in
  grd <- as.data.frame(rbind(upper,lower))
  ## Do the brute-force
  ind=!is.na(y)
  data=as.data.frame(cbind(x[ind],y[ind]))
  names(data)=c("x","y")
  fit <- tryCatch({nls2(formula,data=data,start = grd,algorithm = "brute-force",control=list(maxiter=grid_points))}, error = function(e) {
      return(NA)})
  if(any(is.na(fit))) {
    return(NA)
  }

  #Return the best values
  return(as.list(coef(fit)))
}

#' Integrate a fitted function
#'
#' This function fits the last fitted scan using the models defined previously. Adds the result into fit$result.
#' @param fit The fit object to use
#' @param data The data object
#' @return The fit object
#' @examples
#' integrate_last_scan(fit)
#' @keywords internal
integrate_last_scan <-function(fit,data) {
  x=data[,1]
  y=data[,2]
  models=fit$models
  scanNo=fit$last_scan
  result_line=fit$result[scanNo+1,]
  sde=result_line$std.error

  #Get fitted models
  total_fit=make_fitted_models(models,result_line)

  #Build a total fit model
  total_fit_function=get_fit_function(total_fit)

  #Integrate total fit
  fit_integral=tryCatch({integrate(total_fit_function,lower=fit$integration_range[[1]],upper=fit$integration_range[[2]])$value}, error = function(e){
    #warning(e)
    0})
  #Store value in results table
  fit$result[scanNo+1,'fit_integrated_area']=fit_integral


  #Integrate each model independantly
  for(f in total_fit) {
    #Integrate fitted function
    model_integral=tryCatch({integrate(f[[2]],lower=fit$integration_range[[1]],upper=fit$integration_range[[2]])$value}, error = function(e){
      #warning(e)
      0})
    #Store value in results table
    fit$result[scanNo+1,paste0('integrated_area_',f[[1]])]=model_integral

    #Find std error for each model, based upon their contribution to the overall model:

    #Calculate weighted residuals
    weighted_resids=(f[[2]](x)/total_fit_function(x))*(y-total_fit_function(x))
    weighted_resids=weighted_resids[!is.na(weighted_resids)]
    #Calculate weighted std error
    weighted_sde=sqrt(sum(weighted_resids^2)/length(x))
    #Store values in results table
    fit$result[scanNo+1,paste0('std.error_',f[[1]])]=weighted_sde
  }

  return(fit)
}
