## some setup for MOS field utils
## You can get on option value with, e.g., MOSget('KriegeData') and set it by MOSset('var',value)
## Or use MOS.options$field directly

#' @export
MOS.options <- new.env()

MOS.options$pkg     <- packageName()
MOS.options$version <- packageVersion(MOS.options$pkg)
MOS.options$mapdir  <- "./TMP/naturalearthdata/"
MOS.options$mapdata <- "ne_10m_admin_0_countries"
MOS.options$KriegeData <- 'TMP/KriegeData.RData'
# default ECMWF grid
MOS.options$elon <- seq(-40.00,72.50,by= 0.1)
MOS.options$elat <- seq( 73.50,27.50,by=-0.1)

MOS.options$cov.pars<-c(2.5^2,0.5,0.5) # default sigmasq (C^2), clen (deg), nugget (deg)
MOS.options$altlen <- 150 # altitude range parameter (meters)
MOS.options$trend_model <-  temperature ~ -1

MOS.options$finland.zoom  <- c(19,33,59,71.5) # for plots
MOS.options$alps.zoom <- c(5, 15, 44, 48)

#' Get MOS options
#'
#' Get on option value with, e.g., \code{MOSget('KriegeData')} and set it by \code{MOSset('var',value)}
#' Or use \code{MOS.options$field} directly.
#'
#' @param x The variable to fetch
#' @param default The value for option if not set before
#'
#' @export
MOSget <- function(x=NULL, default=NULL) {
  if (is.null(x)) {

  } else if (exists(x, envir = MOS.options)) {
    get(x,envir = MOS.options)
  } else {
    default
  }
}

#' @export
MOSset <- function(x,y) {
  assign(x, y, envir = MOS.options)
}

