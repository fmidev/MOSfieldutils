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
#' ECdata <- ECMWF_bg_load('file.nc')
#'
#' @export
ECMWF_bg_load<-function(file, elon=MOSget('elon'), elat=MOSget('elat'),
                        variables=NULL,
                        names = NULL,
                        tocelsius = NULL) {

  nlon <- length(elon)
  nlat <- length(elat)
  lonlat <- c("longitude","latitude")

  # default to fetch 2 meter temperature and convert to celsius
  if (is.null(variables)) {
    variables<-c('T_2M')
    names <- c('temperature')
    tocelsius <- c(TRUE)
  }

  # expand names to variables if necessary
  if (length(names) < length(variables)) {
    names[length(names)+1:length(variables)] <- variables[length(names)+1:length(variables)]
  }

  ## assumed order of the data in the netcdf file
  ECdata <- data.frame(longitude=rep(elon,times=nlat),latitude=rep(elat,each=nlon))

  ##  Read ECMWF bg field variables, convert Kelvin to Celsius
  nc <- nc_open(file=file)
  for (i in seq(1,length(variables))) {
    VAR <- ncvar_get(nc,variables[i])
    if (length(tocelsius) >= i) if (tocelsius[i]) VAR <- VAR-273.15
    ECdata[,names[i]] <- c(VAR)
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



# this loads T2, minT, maxT and land sea mask
#' @export
ECMWF_bg_loadminmax<-function(file, elon=MOSget('elon'),elat=MOSget('elat')) {

  nlon <- length(elon)
  nlat <- length(elat)
  lonlat <- c("longitude","latitude")

  nc <- nc_open(file=file)
  T2M <- ncvar_get(nc,'T_2M')-273.15
  Tmin <- ncvar_get(nc,'Tmin_2M')-273.15
  Tmax <- ncvar_get(nc,'Tmax_2M')-273.15
  LSM <- ncvar_get(nc,'LSM')
  nc_close(nc)

  ## transfer bg field to gridded spatial data
  T2<-data.frame(longitude=rep(elon,times=nlat),latitude=rep(elat,each=nlon),
                 temperature=c(T2M),
                 minimumtemperature=c(Tmin),
                 maximumtemperature=c(Tmax),
                 LSM=c(LSM))
  coordinates(T2)<-lonlat
  gridded(T2)<-TRUE
  fullgrid(T2) <- TRUE # ok?
  proj4string(T2)<-CRS("+init=epsg:4326")

  return(T2)
}



# This loads temperature data from a Grib file
# Not completed yet
#' @export
ECMWF_bg_gload<-function(file,msg = 15) {


  lonlat <- c("longitude","latitude")

  ## Read ECMWF bg field, convert Kelvin to Celsius
  gh <- Rgrib2::Ghandle(file,msg)
  out  <- Rgrib2::Gdec(gh)
  out2 <- Rgrib2::Ggrid(gh)

  gt <- GridTopology(cellcentre.offset = c(out2$SW[1],out2$SW[2]),
                     cellsize = c(out2$dx,out2$dy),
                     cells.dim = c(out2$nx,out2$ny))

  # out3<-SpatialGridDataFrame(gt, data.frame(prec=as.vector(out)))
  out3<-SpatialGridDataFrame(gt, data.frame(Temperature=as.vector(out[,dim(out)[2]:1]-273.15)))

  gridded(out3)<-TRUE
  fullgrid(out3) <- TRUE
#  proj4string(out3)<-CRS("+init=epsg:4326")

  Rgrib2::GhandleFree(gh)

  return(out3)
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
MOSstation_csv_load <- function(file,elon=NULL,elat=NULL, skipmiss = TRUE, variable = 'temperature',adddist=FALSE) {
  lonlat <- c("longitude","latitude")
  data <- read.csv(file = file, dec=".", sep = ",")
#  if (skipmiss) data <- data[complete.cases(data), ]
 # now skip according to a specific variable
  if (skipmiss) data <- data[complete.cases(data[,variable]), ]

  ## remove stations outside model region
  if (!is.null(elon)&!is.null(elat))
    data<-data[(data$latitude<=max(elat))&(data$latitude>=min(elat))&(data$longitude>=min(elon))&(data$longitude<=max(elon)),]
  coordinates(data) <- lonlat

  if (adddist) {
    data<-MOS_stations_add_dist(indata=data)
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
# uses spatstat and maptools
#' @export
MOS_stations_add_dist <- function(indata=NULL, infile=NULL, outfile=NULL, distfile=NULL) {

  if (is.null(distfile)) {
    distdata <- MOSgriddata()
  } else {
    distdata <- MOSgrid_load(distfile)
  }
  lonlat <- c("longitude","latitude")
  if (!is.null(infile)) {
    indata <- read.csv(file = infile, dec=".", sep = ",")
  #  indata <- indata[complete.cases(indata), ]
    coordinates(indata) <- lonlat
  }

  np <- spatstat::nncross(maptools::as.ppp.SpatialPointsDataFrame(indata), maptools::as.ppp.SpatialPointsDataFrame(distdata))
  indata$distance <- distdata$distance[np$which]

  if (!is.null(outfile)) {
    write.table(indata, file = outfile,sep=",", row.names=F)
    invisible(indata)
  } else {
    return(indata)
  }

}


# copy files from TEHO
#' @export
MOS_copy_files <- function(fcdate=NULL,fctime="00",leadtime=24,
                           localdir=paste('/var/tmp/',Sys.getenv('USER'),'/',sep=''),
                           copydev=FALSE) {

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

  bgdir <- 'teho:/lustre/tmp/lapsrut/Background_model/Dissemination/Europe/netcdf_kriging/'
  bggdir <- 'teho:/lustre/tmp/lapsrut/Background_model/Dissemination/Europe/grib1/'
  statdir <- 'teho:/lustre/tmp/lapsrut/Projects/POSSE/Station_data/Run/'
  bgdir_minmax <- 'teho:/lustre/tmp/lapsrut/Background_model/Dissemination/Europe/netcdf_kriging_Tmaxmin/'
  statdir_minmax <- 'teho:/lustre/tmp/lapsrut/Projects/POSSE/Station_data/Run_Tmaxmin/'

  bgf <- paste(format(fcdate,format = "%y%j"),fcstr,'000',formatC(leadtime,format="d",flag=0,width=3),sep='')

  bgfg <- paste('F5D',format(fcdate,format = "%m%d"),fcstr,'00',format(fcdate+leadtime*60*60,format = "%m%d%H"),'001',sep='')

  statf <- paste('MOS_',
                 format(fcdate,format="%Y%m%d"),
                 fcstr,'_',format(leadtime),'_',
                 format(fcdate+3600*(leadtime+as.numeric(fcstr)),format="%Y%m%d%H"),
                 '.csv',
                 sep='')

  ecmfw_forecast_file   <- paste(localdir,bgf,sep='')
  ecmfwg_forecast_file   <- paste(localdir,bgfg,sep='')
  station_mos_data_file_in <- paste(localdir,statf,sep='')
  station_mos_data_file <- paste(localdir,'This_timestep.csv',sep='')

  bggcmd <- paste('scp ',bggdir,bgfg,' ',ecmfwg_forecast_file,sep='')

  # development station file (and minmax)
  if (copydev) {
    station_mos_data_file_in <-  station_mos_data_file_in <- paste(localdir,'dev_',statf,sep='')
    statcmd <- paste('scp ',statdir_minmax,statf,' ',station_mos_data_file_in,sep='')
    bgcmd <- paste('scp ',bgdir_minmax,bgf,' ',ecmfw_forecast_file,sep='')
  } else {
    statcmd <- paste('scp ',statdir,statf,' ',station_mos_data_file_in,sep='')
    bgcmd <- paste('scp ',bgdir,bgf,' ',ecmfw_forecast_file,sep='')
  }

  if (!file.exists(ecmfw_forecast_file)) system(bgcmd)
  if (!file.exists(station_mos_data_file_in)) system(statcmd)

  if (!file.exists(ecmfw_forecast_file)) stop('could not copy EC file')
  if (!file.exists(station_mos_data_file_in)) stop('could not copy MOS file')

  return(list(stationfile=station_mos_data_file_in,ecmwffile=ecmfw_forecast_file,
              main= paste(format(fcdate,format="%Y-%m-%d"), fcstr, format(leadtime), 'h forecast'),
              fcdate=fcdate,fctime=as.numeric(fctime),leadtime=leadtime))
}
