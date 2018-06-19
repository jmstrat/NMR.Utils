#' @export
print.nmr.fit.object <- function(x, ...) {

  cat('NMR Fit Object with', length(x$models), 'models:\n')

  hasResult = !all(is.na(x$result))

  for(i in 1:length(x$models)) {
    m = x$models[[i]]
    name = m$name
    if(hasResult) {
      name = paste0(name, ' (', i, ')')
    }
    cat('----------------\n', name, '\n----------------\n')
    print(m)
  }
  if(hasResult) {
    cat('================\n RESULT \n================\n')
    n = nrow(x$result)
    if(n<=10) {
      print(x$result)
    } else {
      print(head(x$result, 10))
      cat('...', n-10, 'rows omitted ...\n')
    }
  }
}

#' @export
print.nmr.fit.model <- function(x, ...) {
  type = x$type_string
  constraint_types = x$constraint_type
  constraints = x$constraint
  has_est = is.function(x$estimation_function)
  initial = x$initial_guess

  lc = length(constraints)
  constraint_table = matrix(NA, 4, lc)
  nam = names(constraints)
  for(j in 1:lc) {
    i = nam[[j]]
    if(constraint_types[[i]] == 'vary') {
      con = c(NA, NA, constraints[[i]], initial[[i]])
    } else if(constraint_types[[i]] == 'range') {
      con = c(constraints[[i]][[1]], constraints[[i]][[2]], NA, initial[[i]])
    } else if(constraint_types[[i]] == 'variable_range') {
      con = c(constraints[[i]][[2]], constraints[[i]][[3]], constraints[[i]][[1]], initial[[i]])
    }
    constraint_table[,j] = con
  }
  ct = as.data.frame(constraint_table)
  colnames(ct) <- names(constraints)
  rownames(ct) <- c('Min','Max','Maxium Change', 'Initial')
  cat('Model:\n')
  cat(type, '\n\n')
  cat('Constraints:\n')
  print(ct)
  if(has_est) {
    cat('\nUsing estimation function:\n')
    print(x$estimation_function)
  }
}
