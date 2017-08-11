## data utilities

## load temperature from ECMWF netcdf file
## assume fixed coordinates in the file

#' Load ECMWF background
#'
#' The functions loads ECMWF background field from a netcfd file.
#' The file is assumed to be in standard format and to have pre defined grid size.
#' The function loads the field temperature and build a gridded SpatialsPixelDataFrame.
#'
#'
#' @param file file to be loaded im netcdf format
#' @param elon the assumed longitude grid in the file
#' @param elat the assumed latitude grid in the file
#'
#' @return SpatialPixelsDataFrame containing a field \code{temperature} in degrees Celsius.
#'
#' @seealso \code{\link{SpatialPixelsDataFrame}}
#'
#' @examples
#' ECMWF_bg_load('file')
#'

#' @export
ECMWF_bg_load<-function(file, elon=seq(-40.00,72.50,by=0.1), elat=seq(73.50,27.50,by=-0.1)) {

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
  proj4string(T2)<-CRS("+init=epsg:4326")

  return(T2)
}

# this loads land sea mask, also
#' @export
ECMWF_bg_load2<-function(file, elon=seq(-40.00,72.50,by= 0.1),elat=seq(73.50,27.50,by=-0.1)) {

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
ECMWF_bg_loadminmax<-function(file, elon=seq(-40.00,72.50,by= 0.1),elat=seq(73.50,27.50,by=-0.1)) {

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



# This loads data from a Grib file
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
  fullgrid(out3) <- TRUE # ok?
#  proj4string(out3)<-CRS("+init=epsg:4326")

  Rgrib2::GhandleFree(gh)

  return(out3)
}


## load MOSsed station values from CSV file
#' @export
MOSstation_cvs_load <- function(file,elon=NULL,elat=NULL, skipmiss = TRUE, variable = 'temperature') {
  lonlat <- c("longitude","latitude")
  data <- read.csv(file = file, dec=".", sep = ",")
#  if (skipmiss) data <- data[complete.cases(data), ]
 # now skip according to a specific variable
  if (skipmiss) data <- data[complete.cases(data[,variable]), ]

  ## remove stations outside model region
  if (!is.null(elon)&!is.null(elat))
    data<-data[(data$latitude<=max(elat))&(data$latitude>=min(elat))&(data$longitude>=min(elon))&(data$longitude<=max(elon)),]
  coordinates(data) <- lonlat
  return(data)
}

# load the MOS grid definition from .Rdata file
#' @export
MOSgrid_load <- function(file=MOSget('KriegeData')) {
  load(file=file)
  return(KriegeData)
}

# Add distance to sea to station data file (from Station_dist.R)
# uses spatstat and maptools
#' @export
MOS_stations_add_dist <- function(infile, outfile, distfile=MOSget('KriegeData')) {

  distdata <- MOSgrid_load(distfile)
  lonlat <- c("longitude","latitude")
  indata <- read.csv(file = infile, dec=".", sep = ",")
#  indata <- indata[complete.cases(indata), ]
  coordinates(indata) <- lonlat

#  np <- spatstat::nncross(spatstat::as.ppp(indata), spatstat::as.ppp(distdata))
  np <- spatstat::nncross(maptools::as.ppp.SpatialPointsDataFrame(indata), maptools::as.ppp.SpatialPointsDataFrame(distdata))
  #  maptools::as.ppp.SpatialPointsDataFrame(distdata)
  indata$dist <- distdata$distance[np$which]

  write.table(indata, file = outfile,sep=",", row.names=F)

}

# Station values on Google map
#' @export
MOS_google_map <- function(stationdata,ECMWFdata,Kriegedata,apikey=NULL) {



  m<-google_map(key=apikey)
  add_markers(m, data = mapdata,
                title  = 'temperature',
                info  = 'info',
                colour = 'colour',
                opacity = 'opacity',
                cluster = TRUE, draggable = FALSE)
  invisible(m)
}


# copy files from TEHO
#' @export
MOS_copy_files <- function(fcdate=format(Sys.time(), "%Y-%m-%d"),fctime="00",leadtime=12,localdir='/var/tmp/mjlaine/',
                           copydev=FALSE) {

#  fctime <- as.POSIXct("2017-08-08")
#  leadtime <- 48
  ## FIXME!!
  fcstr <- formatC(as.numeric(fctime),format="d",flag="0",width =2) # '00' or '12'
  fctime <- as.POSIXct(fcdate)

  bgdir <- 'teho:/lustre/tmp/lapsrut/Background_model/Dissemination/Europe/netcdf/'
  bggdir <- 'teho:/lustre/tmp/lapsrut/Background_model/Dissemination/Europe/grib1/'
  statdir <- 'teho:/lustre/tmp/lapsrut/Projects/POSSE/Station_data/Run/'
  statdir_minmax <- 'teho:/lustre/tmp/lapsrut/Projects/POSSE/Station_data/Run_Tmaxmin/'

  bgf <- paste(format(fctime,format = "%y%j"),fcstr,'000',formatC(leadtime,format="d",flag=0,width=3),sep='')

  bgfg <- paste('F5D',format(fctime,format = "%m%d"),fcstr,'00',format(fctime+leadtime*60*60,format = "%m%d%H"),'001',sep='')

  statf <- paste('MOS_',
                 format(fctime,format="%Y%m%d"),
                 fcstr,'_',format(leadtime),'_',
                 format(fctime+3600*(leadtime+as.numeric(fcstr)),format="%Y%m%d%H"),
                 '.csv',
                 sep='')

  ecmfw_forecast_file   <- paste(localdir,bgf,sep='')
  ecmfwg_forecast_file   <- paste(localdir,bgfg,sep='')
  station_mos_data_file_in <- paste(localdir,statf,sep='')
  station_mos_data_file <- paste(localdir,'This_timestep.csv',sep='')

  bgcmd <- paste('scp ',bgdir,bgf,' ',ecmfw_forecast_file,sep='')
  bggcmd <- paste('scp ',bggdir,bgfg,' ',ecmfwg_forecast_file,sep='')

  # development station file (and minmax)
  if (copydev) {
    station_mos_data_file_in <-  station_mos_data_file_in <- paste(localdir,'dev_',statf,sep='')
    statcmd <- paste('scp ',statdir_minmax,statf,' ',station_mos_data_file_in,sep='')
  } else {
    statcmd <- paste('scp ',statdir,statf,' ',station_mos_data_file_in,sep='')
  }

  if (!file.exists(ecmfw_forecast_file)) system(bgcmd)
  if (!file.exists(station_mos_data_file_in)) system(statcmd)

  if (!file.exists(ecmfw_forecast_file)) stop('could not copy EC file')
  if (!file.exists(station_mos_data_file_in)) stop('could not copy MOS file')

  return(list(stationfile=station_mos_data_file_in,ecmwffile=ecmfw_forecast_file,
              main= paste(format(fctime,format="%Y-%m-%d"), fcstr, format(leadtime), 'h forecast')))
}
