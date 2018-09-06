#' MOSfieldutils: utilitied for gridded MOS data
#'
#' The package provides utilities to work with gridded MOS fields.
#'
#' @section functions:
#' Descriptions still missing, see the individual help items.
#'
#' The DESCRIPTION file:
#' \packageDESCRIPTION{MOSfieldutils}
#' \packageIndices{MOSfieldutils}
#'
#' @author \packageAuthor{MOSfieldutils}
#' Maintainer: \packageMaintainer{MOSfieldutils}
#'
#' @seealso
#' \code{\link[MOSplotting]{MOSplotting}} \code{\link[fastgrid]{fastgrid}}
#'
#' @docType package
#' @name MOSfieldutils
NULL

# methods needed if run by Rscript
#' @import sp ncdf4 fastgrid methods
NULL

## NULL

## MOS data and field manipulations

# now this is package so these are Imported in DESCRIPTION and in NAMESPACE
#require('sp')   # spatial objects
#require('ncdf4') # read netcdf files
#require('maps')
#require('rgdal')  #
#require('raster') # plotting
#require('fastgrid')


#' @export
MOSgrid_old<-function(stationsfile=NULL, modelgridfile=NULL, bgfieldfile=NULL,
                  stations=NULL, modelgrid=NULL, bgfield=NULL,
                  trend_model=NULL,
                  cov.pars = MOSget('cov.pars'),fitpars=FALSE,
                  uselsm=TRUE, usealt = TRUE, altlen = MOSget('altlen'),variable = "temperature",
                  elon=MOSget('elon'),
                  elat=MOSget('elat'),
                  skipmiss = TRUE,
                  distfun = NULL) {

  if (is.null(trend_model))
    trend_model <- as.formula(paste(variable,'~-1'))

  if (is.null(modelgrid)) {
    if (is.null(modelgridfile)) {
      # data("KriegeData", package = MOS.options$pkg, envir = parent.frame())
      modelgrid <- MOSgriddata()
    } else {
      modelgrid <- MOSgrid_load(file=modelgridfile) # loads model grid definition
    }
  }
  if (is.null(stations)) {
    stations <- MOSstation_csv_load(stationsfile,elon=elon,elat=elat,
                                    skipmiss = skipmiss,variable=variable)
  }

  # need distance to sea in LSM, and in some trend_models
  if (is.null(stations$distance) & (uselsm | !is.null(distfun))) {
    stations <- MOS_stations_add_dist(indata = stations)
  }

  if (!is.null(distfun)) {
    stations$distance <- distfun(stations$distance)
    modelgrid$distance <- distfun(modelgrid$distance)
  }

  if (is.null(bgfield)) {
    if (is.null(bgfieldfile))
      bgfield <- NULL
    else
      bgfield <- ECMWF_bg_load(bgfieldfile,elon=elon,elat=elat)
  }

    if (uselsm) {
      LSM  <- as.numeric(!(modelgrid$distance <= 0)) # LSM = (dist > 0)
      LSMy <- as.numeric(!(stations$distance <= 0))
    }
    else {
      LSM<-NULL
      LSMy<-NULL
    }
    if (usealt) {
      ALT <- as.double(modelgrid$elevation)
      ALTy <- as.double(stations$elevation)
    } else {
      ALT <- NULL
      ALTy <- NULL
    }

    if (fitpars) {
      v.fit <- MOSvariofit(stations, cov.pars = cov.pars, trend_model=as.formula(paste(variable,'~1')))
      cov.pars <- MOSvariofitpars(v.fit)
    }

    ypred <- fastgrid::fastkriege(trend_model, data=stations, grid=modelgrid, cov.pars = cov.pars,
                        bg=bgfield,lsm=LSM,lsmy=LSMy,
                        alt=ALT, alty=ALTy, altlen = altlen,
                        variable = variable)

    return(ypred)
}


## function to grid mos stations data to model grid, read data from files or used already loaded data
#' Interpolate MOS station data on a regular grid
#'
#' @param stations spatial data frame for station observations
#' @param bgfield spatial data frame for bakground field
#' @param modelgrid spatial data frame for model grid definition
#'
#' @return SpatialPixelsDataFrame
#'
#' @seealso \code{\link{MOStest}}
#'
#' @examples
#' out <- MOSgrid()
#'
#' @export
MOSgrid<-function(stationsfile=NULL, modelgridfile=NULL, bgfieldfile=NULL,
                  stations=NULL, modelgrid=NULL, bgfield=NULL,
                  trend_model=NULL,
                  cov.pars = MOSget('cov.pars'),fitpars=FALSE,
                  uselsm=TRUE, usealt = TRUE, altlen = MOSget('altlen'),variable = "temperature",
                  elon=MOSget('elon'),
                  elat=MOSget('elat'),
                  skipmiss = TRUE,
                  LapseRate=0.0,
                  distfun = NULL) {

  if (is.null(trend_model))
    trend_model <- as.formula(paste(variable,'~-1'))

  if (is.null(modelgrid)) {
    if (is.null(modelgridfile)) {
      # data("KriegeData", package = MOS.options$pkg, envir = parent.frame())
      modelgrid <- MOSgriddata()
    } else {
      modelgrid <- MOSgrid_load(file=modelgridfile) # loads model grid definition
    }
  }
  if (is.null(stations)) {
    stations <- MOSstation_csv_load(stationsfile,elon=elon,elat=elat,
                                    skipmiss = skipmiss,variable=variable)
  } else {
    # make sure there are no missing values
    # fixme
#    if (skipmiss) {
#      stationdata <- stationdata[complete.cases(as.data.frame(stationdata[,variable])), ]
#    }
  }

  # need distance to sea in LSM, and in some trend_models
  if (is.null(stations$distance) & (uselsm | !is.null(distfun))) {
    stations <- MOS_stations_add_dist(indata = stations)
  }

  if (!is.null(distfun)) {
    stations$distance <- distfun(stations$distance)
    modelgrid$distance <- distfun(modelgrid$distance)
  }

  if (is.null(bgfield)) {
    if (is.null(bgfieldfile))
      bgfield <- NULL
    else
      bgfield <- ECMWF_bg_load(bgfieldfile,elon=elon,elat=elat)
  }

  if (uselsm) {
    LSM  <- as.numeric(!(modelgrid$distance <= 0)) # LSM = (dist > 0)
    LSMy <- as.numeric(!(stations$distance <= 0))
  }
  else {
    LSM<-NULL
    LSMy<-NULL
  }
  if (usealt) {
    ALT <- as.double(modelgrid$elevation)
    ALTy <- as.double(stations$elevation)
  } else {
    ALT <- NULL
    ALTy <- NULL
  }

  if (fitpars) {
    v.fit <- MOSvariofit(stations, cov.pars = cov.pars, trend_model=as.formula(paste(variable,'~1')))
    cov.pars <- MOSvariofitpars(v.fit)
  }

  ypred <- fastgrid::fastkriege(trend_model, data=stations, grid=modelgrid, cov.pars = cov.pars,
                                bg=bgfield,lsm=LSM,lsmy=LSMy,
                                alt=ALT, alty=ALTy, altlen = altlen,
                                variable = variable,LapseRate = LapseRate)


  # copy some attributes from background field to te output
  if (!is.null(bgfield)) {
    attrlist <- MOSget('gribattrlist')
    for (i in 1:length(attrlist)) {
      attr(ypred,attrlist[[i]]) <- attr(bgfield,attrlist[[i]])
    }
  }

  return(ypred)
}

# Default distance transformation (not used now)
MOS_distance_trans <- function(d) {
  ifelse(d<1,100,ifelse(d>50,0,(100-(d/50)*100)))
}

# variogram fitting using gstat package
# you can not use z ~ -1 trend model, it crashes gstat code, z ~ 1 is ok.

#' MOS variogram fit
#'
#' Fits predefined (default exponential) variogram model to MOS station data.
#'
#'
#' @param data dataset as SpatialPixelsDataFrame
#' @param trend_model the assumed trend model
#' @param cov.pars initial values for c(sigmasq, phi ,nugget)
#' @param sill same as sigmasq
#' @param range same as phi
#' @param nugget same as nugget
#'
#' @return variogram model with fitted parameters
#'
#' @section Warning:
#' If the defaults are changed, you shoud check the code carefully.
#'
#' @seealso \code{\link{variogram}}, \code{\link{fit.variogram}}, \code{\link{vgm}}
#'
#' @examples
#' MOSvariofit(stations)
#'
#' @export
MOSvariofit <- function(data, trend_model = temperature~1, plotit=FALSE,
                        vario_model="Exp", cov.pars = NULL,
                        sill=0.5, nugget=0.0 , range=1.0,
                        fit.sigmasq =TRUE, fit.phi = FALSE, fit.nugget =FALSE) {

  if (!is.null(cov.pars)) {
    sill <- cov.pars[1]
    range <- cov.pars[2]
    nugget <- cov.pars[3]
  }

  vario <- gstat::variogram(trend_model, data)
  vmodel <- gstat::vgm(sill, vario_model, range, nugget)
  v.fit <- gstat::fit.variogram(vario, vmodel, fit.ranges=fit.phi, fit.sills = c(fit.nugget,fit.sigmasq), fit.method=1)
  if(attr(v.fit, "singular")) v.fit <- vmodel

  if (plotit) print(plot(vario, v.fit))
  return(v.fit)
  ## check if this is ok if nugget > 0!!!
#  return(c(v.fit[2,2],v.fit[2,3],v.fit[1,2])) # sigmasq, phi ,nugget
}

# extract covariance parameters from variogram fit
#' @export
MOSvariofitpars<- function(fit) {
  return(c(fit[2,2],fit[2,3],fit[1,2])) # sigmasq, phi ,nugget
}

