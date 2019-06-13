read.nmr.bruker.processed.binary <- function(real, imag, proc) {
  warning("Reading binary data is experimental.", call.=FALSE)

  endi <- "little"
  if (proc$bytordp == 1) {
    endi <- "big"
  }

  typ <- "integer"
  if (proc$dtypp == 2) {
    typ <- "numeric"
  }
  jms.classes::log.info("Reading binary data from %s", real)
  data <- readBin(real, what=typ, endian=endi, n=file.info(real)$size)
  if (!is.na(imag)) {
    jms.classes::log.info("Reading binary data from %s", imag)
    ii <- readBin(imag, what=typ, endian=endi, n=file.info(imag)$size)
    data <- complex(real=data, imaginary=ii)
  }

  scale <- tryCatch({
    2^(-proc$nc_proc)
  }, error=function(e) {
    1
  })
  jms.classes::log.debug("Rescaling binary data using a factor of %s", scale)
  data / scale
}

read.nmr.bruker.processed.getppm <- function(acqs, proc, size, dim=1) {
  sw <- acqs$sw_h[[dim]]
  obs <- proc$sf
  car <- (acqs$sfo1 - obs) * 1e6
  delta <- -sw / (size * obs)
  first <- car / obs - delta * size / 2.
  (1:size * delta) + first
}

read.nmr.bruker.processed.binary.1d <- function(real, imag, acqs, proc) {
  jms.classes::log.debug("Reading bruker 1D processed data")
  data <- read.nmr.bruker.processed.binary(real, imag, proc$proc)
  ppm <- read.nmr.bruker.processed.getppm(acqs, proc$proc, length(data))
  # Make data frame
  data <- nmr.data.object(ppm, data)
  # Rename columns
  names(data) <- c("ppm", "intensity")
  return(data)
}

read.nmr.bruker.binary.2d <- function(real, imag, acqs, proc) {
  jms.classes::log.debug("Reading bruker 2D processed data")
  data <- read.nmr.bruker.processed.binary(real, imag, proc$proc)

  jms.classes::log.debug("Deciphering the submatrix...")

  xdim <- c(proc$proc$xdim, proc$proc2$xdim)

  rdata <- matrix(nrow=proc$proc$si, ncol=proc$proc2$si)

  sub_per_dim <- c(
    proc$proc$si / proc$proc$xdim,
    proc$proc2$si / proc$proc2$xdim
  )

  nsubs <- prod(sub_per_dim)

  dim(data) <- c(xdim, nsubs)

  gr <- expand.grid(1:sub_per_dim[[1]], 1:sub_per_dim[[2]])

  # This could probably be more efficient, but for now I'm assuming it's basically magic
  for (i in 1:nsubs) {
    sl <- mapply(function(i, j) {
      c((i - 1) * j, i * j)
    }, gr[i, ], xdim, SIMPLIFY=T)
    sl[1, ] <- sl[1, ] + 1 # 1 based indexing
    rdata[sl[1, 1]:sl[2, 1], sl[1, 2]:sl[2, 2]] <- data[, , i]
  }

  ppm <- read.nmr.bruker.processed.getppm(acqs, proc$proc, nrow(rdata), dim=1)

  # Make data frame
  data <- nmr.data.object(ppm, rdata)
  # Rename columns
  names(data) <- c("ppm", 0:(ncol(data) - 2))
  return(as.nmr2d.data.object(data))
}
