## some setup for MOS field utils
## You can get on option value with, e.g., MOSget('KriegeData') and set it by MOSset('var',value)
## Or use MOS.options$field directly

## you need devtools and roxygen2 packages to build this package, at least from RStudio

#' @rdname MOSget
#' @export
MOS.options <- new.env()

MOS.options$pkg     <- packageName()
MOS.options$version <- packageVersion(MOS.options$pkg)

# default ECMWF grid
MOS.options$elon <- seq(-40.00,72.50,by= 0.1)
MOS.options$elat <- seq( 73.50,27.50,by=-0.1)
# R data to contain EC grid definition
MOS.options$ECMWFgriddata <- 'MOS_EC_grid_1126x461'
# Kriging parameters
MOS.options$cov.pars<-c(2.5^2,0.5,0.5) # default sigmasq (C^2), clen (deg), nugget (deg)
MOS.options$altlen <- 150 # altitude range parameter (meters)
MOS.options$trend_model <-  temperature ~ -1
# lsm values smaller or equal to this are considered as sea
MOS.options$seatreshold <- 0.49
# Default LapseRate K/km, a positive number
MOS.options$LapseRate <- 6.49
# other constants needed in calculations
MOS.options$gravityconstant <- 9.80665
MOS.options$abszeroincelsius <- -273.15

# directories etc (not used now)
MOS.options$mapdir  <- "./TMP/naturalearthdata/"
MOS.options$mapdata <- "ne_10m_admin_0_countries"
MOS.options$KriegeData <- "TMP/KriegeData.RData"

# Plotting
MOS.options$finland.zoom  <- c(19,33,59,71.5)
MOS.options$alps.zoom <- c(5, 15, 44, 48)
MOS.options$norway.zoom <- c(10,25,59,71.5)
MOS.options$canary.zoom <- c(-20,-10,20,34)
MOS.options$canary2.zoom <- c(-19,-15,20,29.5)
MOS.options$scandinavia.zoom <- c(3,33,54,71.5)

# order and names of the coordinates
MOS.options$lonlat <- c("longitude","latitude")

# variable names to grib names and to netcdf names table
# matches the names in MOS CSV file to short_names in ECMFW grib
MOS.options$varnames <- data.frame(
  row.names = c("temperature","minimumtemperature","maximumtemperature","dewpoint","geopotential","lsm"),
  gribname =  c("2t",         "mn2t",              "mx2t",              "2d",      "z",           "lsm"),
  ncname   =  c("T_2M",       "Tmin_2M",           "Tmax_2M",           "D_2M",    "Z",           "LSM"),
  stringsAsFactors = FALSE)
# these might need units conversion
MOS.options$gribtemperatures <- c("2t","mn2t","mx2t","2d")

# grib attributes to save
MOS.options$gribparameters <- c("dataDate","dataTime","timeRangeIndicator","stepUnits","startStep","endStep")

# location of input datasets
MOS.options$ecbgncdir <- 'teho:/lustre/apps/lapsrut/POSSE_GRID/DEV/data/'
MOS.options$ecbggdir <- 'teho:/lustre/apps/lapsrut/POSSE_GRID/DEV/data/EC/'
MOS.options$stationsdir <- 'teho:/lustre/apps/lapsrut/POSSE_GRID/DEV/data/stations/'
MOS.options$bgdir_minmax <- 'teho:/lustre/apps/lapsrut/POSSE_GRID/DEV/data/EC/'
MOS.options$statdir_minmax <- 'teho:/lustre/apps/lapsrut/POSSE_GRID/DEV/data/stations/'

#' MOS options
#'
#' Get on option value with, e.g., \code{MOSget('cov.pars')} and set it by \code{MOSset('var',value)}
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

#' @rdname MOSget
#' @export
MOSset <- function(x,y) {
  assign(x, y, envir = MOS.options)
}

