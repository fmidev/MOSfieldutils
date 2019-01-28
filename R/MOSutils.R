## MOSutils
# mostly command line utilities


# Parse comman line
#' @export
MOS_parse_command_line <- function(args = commandArgs(trailingOnly = TRUE),
                                   default=c("temperature")) {

# library("optparse", quietly=TRUE, verbose=FALSE)

  parser <- optparse::OptionParser()
parser <- optparse::add_option(parser, c("-f","--forecast"), default=NULL,
                     help="Forecast file",
                     dest="ffile")
parser <- optparse::add_option(parser, c("-m","--mos"), default=NULL,
                     help="MOS file",
                     dest="mfile")
parser <- optparse::add_option(parser, c("-a","--analysis"), default=NULL,
                     help="Analysis file",
                     dest="afile")
#
parser <- optparse::add_option(parser, c("-d","--outdir"), default='./TMP/',
                     help="Output directory",
                     dest="outputdir")
parser <- optparse::add_option(parser, c("-o","--out"), default='out',
                     help="Output file base",
                     dest="outputfile")
# -v -q
parser <- optparse::add_option(parser, c("-v", "--verbose"), action="store_true",
                     default=TRUE,
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

return(list(MOSfile=MOSfile, ECfile=ECfile, ECanal=ECanal, outputdir=outputdir, outputfile=outputfile,
            variables=variables, verbose=a$options$verbose))

}
