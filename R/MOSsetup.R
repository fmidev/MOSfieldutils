## some setup for MOS field utils

MOS.options <- new.env()

MOS.options$pkg     <- packageName()
MOS.options$version <- packageVersion(MOS.options$pkg)
MOS.options$mapdir  <- "./TMP/naturalearthdata/"
MOS.options$mapdata <- "ne_10m_admin_0_countries"

MOSget <- function(x, default=NULL) {
  if (exists(x, envir = MOS.options))
    get(x,envir(MOS.options))
  else
    default
}


MOSset <- function(x,y) {
  assign(x, y, envir = MOS.options)
}


#.onAttach <- function(libname, pkgname) {
#  packageStartupMessage(paste("Hello ", packageName(), packageVersion(packageName())))
#}


#.onLoad <- function(libname, pkgname) {
#  op <- options()
#  op.MOSutils <- list(
#    MOSutils.path = "~/R",
#    MOSutils.install.args = "",
#    MOSutils.name = "Your name goes here",
#    MOSutils.desc.author = '"First Last <first.last@example.com> [aut, cre]"',
#    MOSutils.desc.license = "MIT",
#    MOSutils.desc.suggests = NULL,
#    MOSutils.desc = list()
#  )
#  toset <- !(names(op.MOSutils) %in% names(op))
#  if(any(toset)) options(op.MOSutils[toset])
#
#  invisible()
#}
