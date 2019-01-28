## MOSutils
# mostly command line utilities


# Parse comman line
#' @export
MOS_parse_command_line <- function(args = commandArgs(trailingOnly = TRUE),
                                   default=c("temperature")) {

  # library("optparse", quietly=TRUE, verbose=FALSE)

  parser <- optparse::OptionParser(usage = "usage: %prog [options] variables(s)",
    epilogue="Rest of the command line are MOS variable names.
    Example: %prog -m MOS.csv -f ECbg.grib temperature dewpoint\n")
  parser <- optparse::add_option(parser, c("-m","--mos"),
                                 help="MOS csv file",
                                 dest="mfile",action="store")
  parser <- optparse::add_option(parser, c("-f","--forecast"),
                                 help="Forecast grib file",
                                 dest="ffile",action="store")
  parser <- optparse::add_option(parser, c("-a","--analysis"), default=NULL,
                                 help="Analysis grib file [optional]",
                                 dest="afile",action="store")
  #
  parser <- optparse::add_option(parser, c("-d","--outdir"), default='./TMP/',
                                 help="Output directory [%default]",
                                 dest="outputdir")
  parser <- optparse::add_option(parser, c("-o","--out"), default='out',
                                 help="Output file base name [%default]",
                                 dest="outputfile")
  # -v -q
  parser <- optparse::add_option(parser, c("-v", "--verbose"), action="store_true",
                                 default=FALSE,
                                 help="Print extra output [default]")
  parser <- optparse::add_option(parser, c("-q", "--quietly"), action="store_false",
                                 dest="verbose",
                                 help="Print little output")

  a <- optparse::parse_args(parser, args = args, positional_arguments = TRUE)

  MOSfile <- a$options$mfile # MOS stations file name
  ECanal <- a$options$afile  # EC analysis grib file for lsm and z
  ECfile <- a$options$ffile # EC forecast file

  outputdir <- a$options$outputdir # directory for output
  outputfile <- a$options$outputfile # base name for output files

  # rest of the command line are variable names
  variables <- a$args
  if (length(variables) <1) {
    variables <- default
  }

  if (is.null(MOSfile) | is.null(ECfile)) {
    optparse::print_help(parser)
    stop('MOS or EC file name missing!')
  }

  return(list(MOSfile=MOSfile, ECfile=ECfile, ECanal=ECanal, outputdir=outputdir, outputfile=outputfile,
              variables=variables, verbose=a$options$verbose))

}
