#' Phase NMR Data
#'
#' This function phases (complex) NMR data.
#' @param data A (complex) dataframe of NMR data
#' @param p0 0th-order phase correction coefficient
#' @param p1 1st-order phase correction coefficient
#' @param pivot Pivot point for 2nd-order phase correction coefficient (ppm)
#' @return A data frame containing the phased NMR data (ppm,intensity1, ...)
#' @export
#' @examples
#' phase(data, 45, 0, 0)
phase <- function(data, p0, p1, pivot) {
  jms.classes::log.info("Phasing spectra using provided p0, p1 and pivot values")
  ppm <- as.numeric(data[, 1])
  pdata <- as.matrix(data[, 2:ncol(data)])

  n <- nrow(pdata)
  if (is.null(n)) {
    n <- length(pdata)
  }

  if (missing(p1) && missing(pivot)) {
    if (is.null(p0)) {
      return(data)
    }
    if (!all(c("p0", "p1", "pivot") %in% colnames(p0))) stop("Could not determine phase coefficients. Unsupported arguments provided.")
    pivot <- p0[, "pivot"]
    p1 <- p0[, "p1"]
    p0 <- p0[, "p0"]
  } else if (missing(pivot)) {
    if (!all(c("p0", "p1") %in% colnames(p0))) stop("Could not determine phase coefficients. Unsupported arguments provided.")
    pivot <- p1
    p1 <- p0[, "p1"]
    p0 <- p0[, "p0"]
  }

  # 30/5/19 optimisation, changed the following (not 100% tested tho...)
  # x = sapply(pivot, function(p) (1:n - which(abs(ppm-p) == min(abs(ppm-p)))[[1]]) / n )
  # phi = t(mapply(function(pp0, pp1, x_i) pp0 / 180 * pi + pp1 / 180 * pi * x[,x_i], p0, p1, 1:ncol(x) ))
  # result = sapply(1:nrow(phase_vector), function(i) {pdata[,i] * phase_vector[i,]})

  x <- vapply(pivot, function(p) (1:n - which(abs(ppm - p) == min(abs(ppm - p)))[[1]]) / n, numeric(n))

  phi <- p0 / 180 * pi + p1 / 180 * pi * t(x)

  phase_vector <- matrix(complex(real=cos(phi), imaginary=-sin(phi)), nrow(phi), ncol(phi))

  result <- pdata * t(phase_vector)

  data[, 2:ncol(data)] <- result
  return(data)
}

.single_phase <- function(data, p0, p1, pivot) {
  x <- as.numeric(data[, 1])
  y <- as.complex(data[, 2])
  n <- nrow(data)

  # Which can give 2 values if equidistent -- just take the first...
  pivot_points <- which(abs(x - pivot) == min(abs(x - pivot)))[[1]]
  # Between max -1...0...1 centred on pivot
  x <- (1:n - pivot_points) / n
  # Doesn't support vectors for p0 / p1:
  phi <- p0 / 180 * pi + p1 / 180 * pi * x
  phase_vector <- complex(real=cos(phi), imaginary=-sin(phi))
  data[, 2] <- y * phase_vector
  data
}

#' Autophase NMR Data with constraints (using a VERY CRUDE method)
#'
#' This function phases (complex) NMR data.
#' @param spectrum A (complex) dataframe of NMR data (single scan)
#' @param p0_optim_x_range x range over which to optimise the p0 value (ppm)
#' @param p1_optim_x_range x range over which to optimise the p1 value (ppm)
#' @param pivot Pivot point for 2nd-order phase correction coefficient (ppm)
#' @param p0_optim_range range over which the p0 value can vary
#' @param p1_optim_range range over which the p1 value can vary
#' @return A data frame containing the phased NMR data (ppm,intensity)
#' @export
#' @examples
#' apk(data, c(-200, 200), c(1000, 1300), 0, c(-50, 50), c(-5, 0))
#' apk(data)
apk <- function(spectrum, p0_optim_x_range, p1_optim_x_range, pivot, p0_optim_range, p1_optim_range) {
  values <- apk_values(spectrum, p0_optim_x_range, p1_optim_x_range, pivot, p0_optim_range, p1_optim_range)
  jms.classes::log.info("p0 = %s; p1 = %s; pivot = %s", values[["p0"]], values[["p1"]], values[["pivot"]])
  phase(spectrum, values[["p0"]], values[["p1"]], values[["pivot"]])
}

#' @export
#' @rdname apk
apk_values <- function(spectrum, p0_optim_x_range, p1_optim_x_range, pivot, p0_optim_range, p1_optim_range) {
  # We don't need the fancy subsetting additions here, so cast to data.frame for a significant speed boost
  spectrum <- as.data.frame(spectrum)

  if (missing(p0_optim_x_range)) {
    x <- spectrum[, 1]
    xrange <- range(x)
    x_total_range <- diff(xrange)
    m <- mean(xrange)

    x_in_apk0 <- x < m & x > xrange[[1]]
    x_in_apk1 <- x < xrange[[2]] & x > m

    pivot <- x[x_in_apk0][which.max(Re(spectrum[x_in_apk0, 2]))]
    p0_optim_x_range <- pivot + x_total_range * 0.05 * c(-1, 1)

    p1_max <- x[x_in_apk1][which.max(Re(spectrum[x_in_apk1, 2]))]
    p1_optim_x_range <- p1_max + x_total_range * 0.05 * c(-1, 1)

    p0_optim_range <- c(-180, 180)
    p1_optim_range <- c(-180, 180)
  }
  ppm <- spectrum[, 1]
  p0_xmin_points <- which(abs(ppm - p0_optim_x_range[[1]]) == min(abs(ppm - p0_optim_x_range[[1]])))[[1]]
  p0_xmax_points <- which(abs(ppm - p0_optim_x_range[[2]]) == min(abs(ppm - p0_optim_x_range[[2]])))[[1]]
  p1_xmin_points <- which(abs(ppm - p1_optim_x_range[[1]]) == min(abs(ppm - p1_optim_x_range[[1]])))[[1]]
  p1_xmax_points <- which(abs(ppm - p1_optim_x_range[[2]]) == min(abs(ppm - p1_optim_x_range[[2]])))[[1]]
  p0_npoints <- abs(p0_xmax_points - p0_xmin_points)
  p1_npoints <- abs(p1_xmax_points - p1_xmin_points)

  optim_f0 <- function(x) {
    real_phased <- Re(.single_phase(spectrum, x, 0, 0)[p0_xmin_points:p0_xmax_points, 2])
    bsl <- max(c(real_phased[[1]], real_phased[[p0_npoints]]))
    real_phased <- real_phased - bsl
    real_phased <- real_phased / max(real_phased)
    sum(real_phased)
  }
  p0 <- (optimise(optim_f0, lower=p0_optim_range[[1]], upper=p0_optim_range[[2]], maximum=T)$maximum)
  optim_f <- function(x) {
    real_phased <- Re(.single_phase(spectrum, p0, x, pivot)[p1_xmin_points:p1_xmax_points, 2])
    bsl <- max(c(real_phased[[1]], real_phased[[p1_npoints]]))
    real_phased <- real_phased - bsl
    real_phased <- real_phased / max(real_phased)
    sum(real_phased)
  }
  p1 <- optimise(optim_f, lower=p1_optim_range[[1]], upper=p1_optim_range[[2]], maximum=T)$maximum

  return(c(p0=p0, p1=p1, pivot=pivot))
}

#' @param spectra (complex) dataframe containing spectra
#' @export
#' @rdname apk
#' @examples
#' apkpseudo2d(data, c(-200, 200), c(1000, 1300), 0, c(-50, 50), c(-5, 0))
apkpseudo2d <- function(spectra, p0_optim_x_range, p1_optim_x_range, pivot, p0_optim_range, p1_optim_range) {
  jms.classes::log.info("Autophasing 2D spectra scan-by-scan")
  for (i in 2:ncol(spectra)) {
    spectra[, i] <- as.complex(apk(spectra[, c(1, i)], p0_optim_x_range, p1_optim_x_range, pivot, p0_optim_range, p1_optim_range)[, 2])
  }
  return(spectra)
}

#' @export
#' @rdname apk
apkpseudo2d_values <- function(spectra, p0_optim_x_range, p1_optim_x_range, pivot, p0_optim_range, p1_optim_range, .progress=NA) {
  jms.classes::log.info("Autophasing 2D spectra scan-by-scan")
  output <- data.frame(p0=NA, p1=NA, pivot=NA)
  for (i in 2:ncol(spectra)) {
    output[i - 1, ] <- apk_values(spectra[, c(1, i)], p0_optim_x_range, p1_optim_x_range, pivot, p0_optim_range, p1_optim_range)
    if (is.function(.progress)) .progress(i - 1)
  }
  return(output)
}

#' Removes imaginary component from NMR data
#'
#' @param spectra A (complex) dataframe of NMR data
#' @return A data frame containing the real NMR data (ppm,intensity1, ...)
#' @export
#' @examples
#' makeReal(data)
makeReal <- function(spectra) {
  jms.classes::log.debug("Discarding complex part of NMR data")
  if (length(spectra) == 0) {
    return(spectra)
  }
  atts <- attributes(spectra)
  cls <- class(spectra)
  new <- as.data.frame(apply(as.matrix(spectra), 2, Re))
  attributes(new) <- atts
  class(new) <- cls
  return(new)
}
