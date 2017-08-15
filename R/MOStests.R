## some tests using MOS package files

# simple test for loading data, gridding and plotting
#' Test MOS code
#'
#' Load example data sets provided with the package, do gridding and plot results.
#'
#' @examples
#' library(MOSfieldutils)
#' MOStest()
#' @export
MOStest <-function(...) {

  testfiles <- system.file("extdata",package=MOS.options$pkg)

  stationsfile<- system.file("extdata","201706061910_MOS_2017060612_6_2017060618.csv",package=MOS.options$pkg)
  ECMWFbgfile <- system.file("extdata","1715712000006.nc",package=MOS.options$pkg)
  mtitle <- '2016-05-17 12Z 06h forecast'

  stations <- MOSstation_cvs_load(stationsfile)

  out <- MOSgrid(stations = stations,bgfieldfile = ECMWFbgfile)

  main <- paste(mtitle, 'difference to ECMWF')
  s<-MOS_plot_field(out,layer="diff",stations=stations,main=main,...)
  print(s)

  invisible(out)
}


# do some more comprehensive test on recent files
#' @export
MOStestsuite1 <- function(files,savedir='/tmp/',saveplots=!interactive(),...) {

  cat('Calculating...')
  out1 <- MOSgrid(stationsfile = files$stationfile,bgfieldfile  = files$ecmwffile)

  filefun <- function(f) if (saveplots) paste(savedir,f,sep='') else NULL

  cat('plotting')
  s <- MOS_plot_field(out1,layer="diff",main=f$main,pngfile=filefun('MOStest1diff.png'),...)
  print(s)
  s <- MOS_plot_field(out1,layer="diff",zoom=MOS.options$finland.zoom,
                 main=f$main,
                 pngfile=filefun('MOStest1fin.png'),...)
  print(s)
}
