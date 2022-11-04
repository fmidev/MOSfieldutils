## data utilities

## load temperature from ECMWF netcdf file
## assume fixed coordinates in the file

#' Load ECMWF background
#'
#' The functions loads ECMWF background field from a netcfd file.
#' The file is assumed to be in standard format and to have pre-defined grid size.
#' The function loads the given fields and builds a SpatialsGridDataFrame.
#'
#'
#' @param file file to be loaded in netcdf format
#' @param elon the assumed longitude grid in the file
#' @param elat the assumed latitude grid in the file
#'
#' @return SpatialGridDataFrame containing fields given in parameter \code{variables}.
#'
#' @seealso \code{\link{SpatialGridDataFrame}}
#'
#' @examples
#' ECdata <- ECMWF_bg_load('file.nc',
#'              variables=c('T_2M','Tmin_2M','Tmax_2M'),
#'              varnames=c('temperature','minimumtemperature','maximumtemperature'),
#'              tocelsius=c(TRUE,TRUE,TRUE))
#'
#' @export
ECMWF_bg_load<-function(file, elon=MOSget('elon'), elat=MOSget('elat'),
                        variables=NULL,
                        varnames = NULL,
                        tocelsius = NULL,
                        extra = TRUE) {

  nlon <- length(elon)
  nlat <- length(elat)
  lonlat <- c("longitude","latitude")

  # default to fetch 2 meter temperature and convert to celsius
  # need to change "names" here
  if (is.null(variables)) {
    variables<-c('T_2M')
    varnames <- c('temperature')
    tocelsius <- c(TRUE)
  }

  # expand names to variables if necessary
  if (length(varnames) < length(variables)) {
    names[length(varnames)+1:length(variables)] <- variables[length(varnames)+1:length(variables)]
  }

  ## assumed order of the data in the netcdf file
  ECdata <- data.frame(longitude=rep(elon,times=nlat),latitude=rep(elat,each=nlon))

  ##  Read ECMWF bg field variables, convert Kelvin to Celsius
  nc <- nc_open(file=file)
  ncnames <- names(nc$var)
  for (i in seq(1,length(variables))) {
    VAR <- ncvar_get(nc,variables[i])
    if (length(tocelsius) >= i) if (tocelsius[i]) VAR <- VAR-273.15
    ECdata[,varnames[i]] <- c(VAR)
  }

  # load some extra variables, LSM and geopotential
  if (extra) {
    if (!is.na(match(TRUE,'LSM'==ncnames))) {
      ECdata$LSM<-c(ncvar_get(nc,'LSM'))
    }
    if (!is.na(match(TRUE,'Z'==ncnames))) {
      ECdata$geopotential<-c(ncvar_get(nc,'Z'))
    }
  }

  nc_close(nc)

  ## transfer bg field to gridded spatial data
  coordinates(ECdata)<-lonlat
  gridded(ECdata)<-TRUE
  fullgrid(ECdata) <- TRUE # ok? makes the variable smaller
  proj4string(ECdata)<-sp::CRS("+init=epsg:4326")

  return(ECdata)
}


# initial version
#' @export
ECMWF_bg_load_old<-function(file, elon=MOSget('elon'), elat=MOSget('elat')) {

  nlon <- length(elon)
  nlat <- length(elat)
  lonlat <- c("longitude","latitude")

  ## Read ECMWF bg field, convert Kelvin to Celsius
  nc <- nc_open(file=file)
  T2M <- ncvar_get(nc,'T_2M')-273.15
  nc_close(nc)

  ## transfer bg field to gridded spatial data
  T2<-data.frame(longitude=rep(elon,times=nlat),latitude=rep(elat,each=nlon),temperature=c(T2M))
  coordinates(T2)<-lonlat
  gridded(T2)<-TRUE
  fullgrid(T2) <- TRUE # ok?
  proj4string(T2)<-sp::CRS("+init=epsg:4326")

  return(T2)
}

# this loads land sea mask, also
#' @export
ECMWF_bg_load2<-function(file, elon=MOSget('elon'),elat=MOSget('elat')) {

  nlon <- length(elon)
  nlat <- length(elat)
  lonlat <- c("longitude","latitude")

  nc <- nc_open(file=file)
  T2M <- ncvar_get(nc,'T_2M')-273.15
  LSM <- ncvar_get(nc,'LSM')
  nc_close(nc)

  ## transfer bg field to gridded spatial data
  T2<-data.frame(longitude=rep(elon,times=nlat),latitude=rep(elat,each=nlon),
                 temperature=c(T2M),
                 LSM=c(LSM))
  coordinates(T2)<-lonlat
  gridded(T2)<-TRUE
  fullgrid(T2) <- TRUE # ok?
  proj4string(T2)<-CRS("+init=epsg:4326")

  return(T2)
}



# this loads T2, minT, maxT and land sea mask (and geopotential and dewpoint)
# should use ECMWF_bg_load instead
#' @export
ECMWF_bg_loadminmax<-function(file, elon=MOSget('elon'),elat=MOSget('elat')) {

  nlon <- length(elon)
  nlat <- length(elat)
  lonlat <- c("longitude","latitude")

  nc <- nc_open(file=file)
  varnames <- names(nc$var)
  T2M <- ncvar_get(nc,'T_2M')-273.15
  Tmin <- ncvar_get(nc,'Tmin_2M')-273.15
  Tmax <- ncvar_get(nc,'Tmax_2M')-273.15
  LSM <- ncvar_get(nc,'LSM')

  ## transfer bg field to gridded spatial data
  T2<-data.frame(longitude=rep(elon,times=nlat),latitude=rep(elat,each=nlon),
                 temperature=c(T2M),
                 minimumtemperature=c(Tmin),
                 maximumtemperature=c(Tmax),
                 LSM=c(LSM))

  # these are optional and checked right now, change this later
  if (!is.na(match(TRUE,'Z'==varnames))) {
    geopotential <- ncvar_get(nc,'Z')
    T2$geopotential<-c(geopotential)
  }
  if (!is.na(match(TRUE,'D_2M'==varnames))) {
    dewpoint <- ncvar_get(nc,'D_2M')
    T2$dewpoint <- c(dewpoint)
  }

  nc_close(nc)

  coordinates(T2)<-lonlat
  gridded(T2)<-TRUE
  fullgrid(T2) <- TRUE # ok?
  proj4string(T2)<-CRS("+init=epsg:4326")

  return(T2)
}



# This loads model data data from a Grib file
#' Load ECMWF background grib files
#'
#' The functions loads ECMWF background field from a grib file.
#' The file is assumed to be in standard format and to have pre-defined grid size.
#' The function loads the given fields and builds a SpatialsGridDataFrame.
#'
#'
#' @param file the forecast file to be loaded in grib format
#' @param analysis optional analysis file for reading geopotential and land-sea-mask
#' @param variables character list of grib variables to be read, use short grib names
#' @param varnames names of the variables in the output, from short grib name to full names
#'
#' @return SpatialGridDataFrame containing fields given in parameter \code{variables}.
#'
#' @seealso \code{\link{SpatialGridDataFrame}}
#'
#' @examples
#' ECdata <- ECMWF_bg_gload('file.grib')
#'
#' @export
ECMWF_bg_gload<-function(file,analysis=NULL, variables = NULL, varnames=NULL, tocelsius=TRUE) {


  lonlat <- c("longitude","latitude")

  ## Read ECMWF bg field, convert Kelvin to Celsius

  # default to fetch 2 meter temperature and convert to celsius
  if (is.null(variables)) {
    variables<-c('2t')
    varnames <- c('temperature')
  }

  # expand names to variables if necessary
  if (length(varnames) < length(variables)) {
    varnames[length(varnames)+1:length(variables)] <- variables[length(varnames)+1:length(variables)]
  }

  # these are saved
  GribPar <- MOSget("gribparameters")
  IntPar <- c("Nx", "Ny", "iScansNegatively", "jScansPositively",
              "jPointsAreConsecutive",
              "alternativeRowScanning","missingValue", "numberOfMissing",
              GribPar)
  StrPar <- c("units")

  g <- Rgrib2::Gopen(file)

  for (i in seq(1,length(variables))) {

    m <- Rgrib2::Gfind(g,variables[i])
    gh <- Rgrib2::Ghandle(g,m)
    gdat  <- Rgrib2::Gdec(gh)
    ginf <- Rgrib2::Ginfo(gh, IntPar = IntPar, StrPar = StrPar)

    if (ginf$jScansPositively==0){
      gdat <- gdat[,dim(gdat)[2]:1]
    }

    if (ginf$units=="K" & tocelsius){
      gdat <- converttocelsius(gdat)
    }

    if (i==1) { # generate output data
      gdom <- Rgrib2::Gdomain(gh)
      gt <- sp::GridTopology(cellcentre.offset = c(gdom$SW[1],gdom$SW[2]),
                       cellsize = c(gdom$dx,gdom$dy),
                       cells.dim = c(gdom$nx,gdom$ny))

      out<-sp::SpatialGridDataFrame(gt, data.frame(VAR1=as.vector(gdat)))
      names(out) <- varnames[i]
      sp::coordnames(out) <- MOSget('lonlat')
      sp::gridded(out)<-TRUE
      sp::fullgrid(out) <- TRUE
      sp::proj4string(out)<-sp::CRS("+init=epsg:4326")
    }
    else {
      out@data[,varnames[i]] <- as.vector(gdat)
    }

    attr(out@data[,varnames[i]],'gribattr') <- as.list(ginf[GribPar])

    Rgrib2::GhandleFree(gh)

  }

  # read analysis file for altitude
  if (!is.null(analysis)) {

    ga <- Rgrib2::Gopen(analysis)

    avariables <- c('z','lsm')
    avarnames <- c('geopotential','lsm')

    for (i in seq(1,length(avariables))) {

      m <- Rgrib2::Gfind(ga,avariables[i])
      gh <- Rgrib2::Ghandle(ga,m)
      gdat  <- Rgrib2::Gdec(gh)

      ginf <- Rgrib2::Ginfo(gh, IntPar = IntPar, StrPar = StrPar)

      if (ginf$jScansPositively==0){
        gdat <- gdat[,dim(gdat)[2]:1]
      }

#      if (ginf$units=="K" & tocelsius){
#        gdat <- converttocelsius(gdat)
#      }

      out@data[,avarnames[i]] <- as.vector(gdat)
    }

    attr(out@data[,avarnames[i]],'gribattr') <- list(dataDate=ginf$dataDate,
                                                     dataTime=ginf$dataTime,
                                                     stepRange=ginf$stepRange)

    attr(out,'steplist_anal') <- ginf[c("startStep","endStep")]

    Rgrib2::GhandleFree(gh)
  }

  # calculate elevation as geopotential height in meters
  if (!is.null(out$geopotential)) {
    out$elevation <- geopotential2meters(out$geopotential)
  }

  return(out)
}


## load MOSsed station values from CSV file
#' Load station data from CSV file
#' @param file Station data file in CSV format
#'
#' @param elon model grid longitudes, if given the stations outside the grid are cut off
#' @param elat model grid latitudes
#' @param skipmiss do we drop row with missing values for \code{variable}
#' @param variable the variable name used for cheking missing values
#' @param adddist do we add distance to sea as a new variable using \code{MOS_stations_add_dist}
#'
#' @export
MOSstation_csv_load <- function(file,elon=NULL,elat=NULL, skipmiss = TRUE, variable = 'temperature',
                                adddist=FALSE,olddist=FALSE) {
  lonlat <- c("longitude","latitude")
  data <- read.csv(file = file, dec=".", sep = ",")
#  if (skipmiss) data <- data[complete.cases(data), ]
 # now skip according to a specific variable
  if (skipmiss) data <- data[complete.cases(data[,variable]), ]

  ## remove stations outside model region
  if (!is.null(elon)&!is.null(elat))
    data<-data[(data$latitude<=max(elat))&(data$latitude>=min(elat))&(data$longitude>=min(elon))&(data$longitude<=max(elon)),]
  sp::coordinates(data) <- lonlat
  sp::proj4string(data) <- sp::CRS("+init=epsg:4326")

  if (adddist) {
    data<-MOS_stations_add_dist(indata=data,olddist = olddist)
  }

  return(data)
}

# load the MOS grid definition from .Rdata file
# CHECK THIS!!
#' @export
MOSgrid_load <- function(file) {
  # o<-data(MOSget('ECMWFgriddata'),package=MOSget('pkg'))
  # griddata <- get(o)
  o <- load(file=file)
  #return(KriegeData)
  # return(griddata)
  get(o)
}

# return the default MOS grid definition data
#' @export
MOSgriddata <- function() {
  return(get(MOSget('ECMWFgriddata')))
}

# Add distance to sea to station data file (from Station_dist.R)
# uses spatstat and maptools (NOT ANYMORE)
#' @export
MOS_stations_add_dist <- function(indata=NULL, infile=NULL, outfile=NULL, distfile=NULL,olddist=FALSE) {

  if (olddist) {
    distdata <- get('Spatial_grid_elev_dist_Matti_Polster_NEW')
  } else if (is.null(distfile)) {
    distdata <- MOSgriddata()
  }
  else {
    distdata <- MOSgrid_load(distfile)
  }
  lonlat <- c("longitude","latitude")
  if (!is.null(infile)) {
    indata <- read.csv(file = infile, dec=".", sep = ",")
  #  indata <- indata[complete.cases(indata), ]
    coordinates(indata) <- lonlat
    proj4string(data) <- sp::CRS("+init=epsg:4326")
  }

#  np <- spatstat::nncross(maptools::as.ppp.SpatialPointsDataFrame(indata), maptools::as.ppp.SpatialPointsDataFrame(distdata),
#                          what="which")
  if (olddist) {
    #indata$distance <- distdata$distance[np]
    indata$distance <- c(fastgrid::grid2points(distdata, indata, "distance", method="bilinear"))
  }
  else
  {
    #indata$distance <- distdata$distance[np]/1000 # from meters to kilometers!!!
    indata$distance <- c(fastgrid::grid2points(distdata, indata, "distance", method="bilinear")) / 1000
  }

  #remove NA's
  indata <- indata[!is.na(indata$distance), ]

  if (!is.null(outfile)) {
    write.table(indata, file = outfile,sep=",", row.names=F)
    invisible(indata)
  } else {
    return(indata)
  }

}


# Add lsm variable to stations data by bilinear interpolation
#' @export
MOS_stations_add_lsm <- function(stations, griddata, variable='lsm', method='bilinear') {
  stations$lsm  <- fastgrid::grid2points(griddata,stations,variable=variable,method = method)
  return(stations)
}

# copy files from TEHO
#' @export
MOS_copy_files <- function(fcdate=NULL,fctime="00",leadtime=24,
                           localdir=paste('/var/tmp/',Sys.getenv('USER'),'/',sep=''),
                           copydev=FALSE,grib=TRUE) {

  # try to copy the latest files
  if (is.null(fcdate)) {
    now<-Sys.time()
    hour <- as.numeric(format(now, "%H",tz="EET"))
    if (hour < 9) {
      fcdate <- format(now-3600*24, "%Y-%m-%d")
      fctime <- "12"
#      leadtime <- 24
    } else if (hour < 18) {
      fcdate <- format(now, "%Y-%m-%d")
      fctime <- "00"
#      leadtime <- 36
    } else {
      fcdate <- format(now, "%Y-%m-%d")
      fctime <- "12"
#      leadtime <- 24
    }
    cat(paste(format(fcdate,format="%Y-%m-%d"), fctime, format(leadtime), 'h forecast'),'\n')
  }

  fcstr <- formatC(as.numeric(fctime),format="d",flag="0",width =2) # '00' or '12'
  fcdate <- as.POSIXct(fcdate)

  bgdir <- MOSget('ecbgncdir')
  bggdir <- MOSget('ecbggdir')
  statdir <- MOSget('stationsdir')
  bgdir_minmax <- MOSget('bgdir_minmax')
  statdir_minmax <- MOSget('statdir_minmax')

  bgf <- paste(format(fcdate,format = "%y%j"),fcstr,'000',formatC(leadtime,format="d",flag=0,width=3),sep='')

  bgfg <- paste('F5D',
                format(fcdate,format = "%m%d"),
                fcstr,'00',
                format(fcdate+(leadtime+as.numeric(fcstr))*60*60,format = "%m%d%H"),'001',sep='')
  # analysis file
  bgfga <- paste('F5D',
                 format(fcdate,format = "%m%d"),
                 fcstr,'00',
                 format(fcdate+(0+as.numeric(fcstr))*60*60,format = "%m%d%H"),'001',sep='')
  # station MOS data file
  statf <- paste('MOS_',
                 format(fcdate,format="%Y%m%d"),
                 fcstr,'_',format(leadtime),'_',
                 format(fcdate+3600*(leadtime+as.numeric(fcstr)),format="%Y%m%d%H"),
                 '.csv',
                 sep='')

  ecmfw_forecast_file   <- paste(localdir,bgf,sep='')
  ecmfwg_forecast_file   <- paste(localdir,bgfg,sep='')
  ecmfwga_forecast_file   <- paste(localdir,bgfga,sep='')
  station_mos_data_file_in <- paste(localdir,statf,sep='')
  station_mos_data_file <- paste(localdir,'This_timestep.csv',sep='')

  bggcmd <- paste('scp ',bggdir,bgfg,' ',ecmfwg_forecast_file,sep='')
  bggacmd <- paste('scp ',bggdir,bgfga,' ',ecmfwga_forecast_file,sep='')

  # development station file (and minmax)
  if (copydev) {
    station_mos_data_file_in <-  station_mos_data_file_in <- paste(localdir,'dev_',statf,sep='')
    statcmd <- paste('scp ',statdir_minmax,statf,' ',station_mos_data_file_in,sep='')
    bgcmd <- paste('scp ',bgdir_minmax,bgf,' ',ecmfw_forecast_file,sep='')
  } else {
    statcmd <- paste('scp ',statdir,statf,' ',station_mos_data_file_in,sep='')
    bgcmd <- paste('scp ',bgdir,bgf,' ',ecmfw_forecast_file,sep='')
  }

  if (!file.exists(station_mos_data_file_in)) system(statcmd)
  if (!file.exists(station_mos_data_file_in)) stop('could not copy MOS file')

  if (grib) {
    if (!file.exists(ecmfwg_forecast_file)) system(bggcmd)
    if (!file.exists(ecmfwga_forecast_file)) system(bggacmd)
    if (!file.exists(ecmfwg_forecast_file)) stop('could not copy EC grib file')
    if (!file.exists(ecmfwga_forecast_file)) stop('could not copy EC grib analysis file')
  } else {
    if (!file.exists(ecmfw_forecast_file)) system(bgcmd)
    if (!file.exists(ecmfw_forecast_file)) stop('could not copy EC file')
  }

  out <- list(stationfile=station_mos_data_file_in,ecmwffile=ecmfw_forecast_file,
               main= paste(format(fcdate,format="%Y-%m-%d"), fcstr, format(leadtime), 'h forecast'),
               fcdate=fcdate,fctime=as.numeric(fctime),leadtime=leadtime)
  if (grib) {
    out$ecmwffile <- ecmfwg_forecast_file
    out$ecmwfafile <- ecmfwga_forecast_file
  }
  return(out)
}



# take SpatialGrid and save it as a grib file, level=0 only now

#' Save SpatialGridDataFrame as ECMWF grib data
#'
#' The functions takes loads SpatialsGridDataFrame as input and tries to save it in a
#' grib file.
#'
#'
#' @param g input spatial grid
#' @param file the output file in grib format
#' @param variables character list of variables to be saved, use R name
#' @param varnames names of the variables in the output, use short grib names
#'
#' @return This function does not return any value-
#'
#' @seealso \code{\link{SpatialGridDataFrame}}
#'
#' @examples
#' sptogib(output,file='file.grib',variables="dewpoint",varnames="2d"))
#'
#' @export
sptogrib <- function(g,file,variables=NULL,varnames=NULL,gribformat=1,sample='regular_ll_sfc_grib1',
                     tokelvin=FALSE) {

  if (is.null(variables)) {
    variables <- names(g)
    varnames <- variables
  }

  if (is.null(varnames)) {
    stop('you must provide variable names')
    variables <- names(g)
    varnames <- variables
  }

  # create geodomain from Grid topology
  gt <- sp::getGridTopology(g)
  d <- list(projection=list(proj="latlong",lon0=0),nx=gt@cells.dim[1],
            ny=gt@cells.dim[2],
            SW=gt@cellcentre.offset,
            NE=gt@cellcentre.offset + gt@cellsize*(gt@cells.dim-1),
            dx=gt@cellsize[1],dy=gt@cellsize[2])
  # swap latitude order as we have jScansPositively=0 and Rgrib2::Gmod wants to do swapping
  dummy <- d$SW[2]; d$SW[2]<-d$NE[2];d$NE[2]<-dummy
  attr(d,"class") <- "geodomain"

  GribPar <- MOSget("gribparameters")

  # extract data and transform it
  for (i in 1:(dim(g@data))[2]) {
    ii <- match(TRUE,names(g)[i]==variables)
    if (is.na(ii)) {
      next
    } else {
      gname <- varnames[ii]
    }
    x <- matrix(g@data[,i],nrow=d$nx,byrow=FALSE)
    x <- x[,dim(x)[2]:1] # this done again in Rgrib2::Gmod !!!
    attr(x,'domain')<-d

    if (tokelvin & (gname %in% MOSget('gribtemperatures'))) {
      x <- converttokelvin(x)
    }
    IntPar <- list(typeOfLevel=1,level=0,jScansPositively=0)
    if (!tokelvin & (gname %in% MOSget('gribtemperatures'))) {
      StrPar <- list(shortName=gname,units="C")
    }
    else {
      StrPar <- list(shortName=gname)
    }
    # Grib attributes
    ginf <- attr(g@data[,i],'gribattr')
    if (!is.null(ginf)) {
      IntPar <- c(IntPar,ginf)
    }

    gnew <- Rgrib2::Gcreate(d, edition=gribformat)
    Rgrib2::Gmod(gnew, data = x, StrPar=StrPar, IntPar=IntPar)
    if (i==1) appe = FALSE
    else appe = TRUE
    Rgrib2::Gwrite(gnew,file=file,append=appe)
    Rgrib2::GhandleFree(gnew)
  }
  invisible(NULL)

}


# convert from Kelvin to celsius and barf if there seems to be a problem
converttocelsius <- function(x,check=TRUE) {
  y <- x - 273.15
  if (check) {
    if (any(y < -200)) {
      stop('Are you sure that the temperature conversion is needed here! Stopping now.')
    }
  }
  return(y)
}

# convert from celsius to Kelvin and barf if there seems to be a problem
converttokelvin <- function(x,check=TRUE) {
  if (check) {
    if (any(x > 100)) {
      stop('Are you sure that the temperature conversion is needed here! Stopping now.')
    }
  }
  y <- x + 273.15
  return(y)
}

# geopotential to geopotential height in meters
geopotential2meters <- function(x) {
  g <- MOS.options$gravityconstant
  if (is.null(g)) stop('problems with constants')
  y <- x / g
  return(y)
}

