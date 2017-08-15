## some tests using package files


MOStest <-function() {

  testfiles <- system.file("extdata",package="MOSfieldutils")

  stationdata <- system.file("extdata","201706061910_MOS_2017060612_6_2017060618.csv",package="MOSfieldutils")
  ECbg <- system.file("extdata","1715712000006.nc",package="MOSfieldutils")

  out <- MOSgrid(stationdata=stationdata,bgfile = ECbg)

  MOS_plot_field(out,layer="diff")

  out
}
