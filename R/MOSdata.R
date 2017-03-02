## data utilities

## load temperature from ECMWF netcdf file
## assume fixed coordinates in the file
ECMWF_bg_load<-function(file, elon=seq(-40.00,72.50,by= 0.1), elat=seq(73.50,27.50,by=-0.1)) {

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
  proj4string(T2)<-CRS("+init=epsg:4326")

  return(T2)
}

# this loads land sea mask, also
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
  proj4string(T2)<-CRS("+init=epsg:4326")

  return(T2)
}

## load MOSsed station values from CSV file
MOSstation_cvs_load <- function(file,elon=NULL,elat=NULL) {
  lonlat <- c("longitude","latitude")
  data <- read.csv(file = file, dec=".", sep = ",")
  data <- data[complete.cases(data), ]
  ## remove stations outside model region
  if (!is.null(elon)&!is.null(elat))
    data<-data[(data$latitude<=max(elat))&(data$latitude>=min(elat))&(data$longitude>=min(elon))&(data$longitude<=max(elon)),]
  coordinates(data) <- lonlat
  return(data)
}

# load the MOS grid definition from .Rdata file
MOSgrid_load <- function(file='KriegeData.Rdata') {
  load(file=file)
  return(KriegeData)
}
