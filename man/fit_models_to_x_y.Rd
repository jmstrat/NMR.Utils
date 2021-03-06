% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FIT-leastSquares.R
\name{fit_models_to_x_y}
\alias{fit_models_to_x_y}
\title{Run a single least-squares fit}
\usage{
fit_models_to_x_y(models, x, y, control, brute_force = FALSE,
  brute_force_grid_size = 1)
}
\arguments{
\item{models}{The models to use}

\item{x}{The x data to fit}

\item{y}{The y data to fit}

\item{control}{The nls control parameters}

\item{brute_force}{Should brute force the starting parameters}

\item{brute_force_grid_size}{Number of parameters to try for every constraint during the brute force.}
}
\value{
The least-squares result
}
\description{
This function fits a single scan using the models provided.
}
\examples{
fit_models_to_x_y(models, x, y)
}
\keyword{internal}
