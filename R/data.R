## describe data here

# MOS_EC_grid_1126x461

#' MOS grid definitions
#'
#' A dataset containing \code{\link[sp]{SpatialGridDataFrame}} that defines the MOS grid, i.e. the regular longitude-latitude
#' grid that the point station values are interpolated into. The grid size is 1126x461 correspondig to 0.1° spacing.
#'
#' @format A SpatialGridDataFrame with 519086 (7.9 MB) elements as
#' \describe{
#'   \item{elevation}{altitude of the grid point from the mean sea level in meters}
#'   \item{distance}{distance from the closest sea point in meters}
#' }
#'
#' The coordinates (grid pixel center points) are defined as:
#' #' \describe{
#'   \item{longitude}{seq(-40.00,72.50,by= 0.1), 1126 elements}
#'   \item{latitude}{seq( 73.50,27.50,by=-0.1), 461 elements}
#' }
#'
#' @source Matti Horttanainen FMI
"MOS_EC_grid_1126x461"

# Old name
# "KriegeData"
