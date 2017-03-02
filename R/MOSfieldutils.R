### MOS data and field manipulations

# now this is package so these are required in DESCRIPTION
#require('sp')   # spatial objects
#require('ncdf4') # read netcdf files
#require('maps')
#require('rgdal')  #
#require('raster') # plotting
#require('fastgrid')

## function to grid mos stations data to model grid, read data from files
MOSgrid<-function(stationsfile, modelgridfile, bgfile=NULL, trend_model=NULL,
                  cov.pars = c(6.0,1.0,0.0),
                  uselsm=TRUE, variable = "temperature",
                  elon=seq(-40.00,72.50,by= 0.1),
                  elat=seq(73.50,27.50,by=-0.1)) {

    if (is.null(trend_model))
        trend_model <- temperature ~ -1

    KriegeData <- MOSgrid_load(file=modelgridfile) # loads KriegeData
    stationdata <- MOSstation_cvs_load(stationsfile,elon,elat)

    distfun<-function(d) ifelse(d < 1, 100, ifelse(d > 50, 0, (100 - (d/50) * 100)))
    stationdata$dist2 <- distfun(stationdata$dist)
    KriegeData$dist2<-distfun(KriegeData$distance)

    if (is.null(bgfile))
      bg <- NULL
    else
      bg <- ECMWF_bg_load(bgfile,elon=elon,elat=elat)

    if (uselsm) {
      LSM  <- as.numeric(!(KriegeData$distance <= 0)) # LSM = (dist > 0)
      LSMy <- as.numeric(!(stationdata$dist<=0))
    }
    else {
      LSM<-NULL
      LSMy<-NULL
    }

    ypred <- fastkriege(trend_model, data=stationdata, grid=KriegeData, cov.pars = cov.pars,
                        bg=bg,lsm=LSM,lsmy=LSMy,variable = variable)

    return(ypred)
}

# variogram fitting using gstat package
# you can not use z ~ -1 trend model, it crashes gstat code, z ~ 1 is ok.
MOSvariofit <- function(data, trend_model = temperature~1, plotit=FALSE,
                        vario_model="Exp", cov.pars = NULL,
                        sill=0.5, nugget=0.0 , range=1.0) {

  if (!is.null(cov.pars)) {
    sill <- cov.pars[1]
    range <- cov.pars[2]
    nugget <- cov.pars[3]
  }

  vario <- variogram(trend_model, data)
  vmodel <- vgm(sill, vario_model, range, nugget)
  v.fit <- fit.variogram(vario, vmodel, fit.ranges=FALSE, fit.sills = c(FALSE, TRUE), fit.method=1)
  if(attr(v.fit, "singular")) v.fit <- vmodel

  if (plotit) print(plot(vario, v.fit))
  return(v.fit)
}

# utility for SpatialPixelsDataFrame coordinates
gridlon <- function(x) coordinatevalues(getGridTopology(x))$longitude
gridlat <- function(x) coordinatevalues(getGridTopology(x))$latitude

## map grid values to points (uses H from fastgrid)
grid2points<-function(grid,data,variable="temperature"){
  grid.grid <- getGridTopology(grid)
  elon<-coordinatevalues(grid.grid)$longitude
  elat<-coordinatevalues(grid.grid)$latitude
  H<-f90Hmat(elon,elat,coordinates(data))
  y <- as.matrix(H%*%as.matrix(grid@data[,variable]))
  return(y)
}
