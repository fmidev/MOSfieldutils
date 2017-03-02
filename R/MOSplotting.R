## plotting utilities for MOS data

## plot_MOS_field to plot the MOS field
# plot SpatialPixelsDataFrame
plot_MOS_field <- function(g,layer=1,main="",cmin=-40,cmax=40,ncolors=100,
                           plot.europe=TRUE,zoom=NULL,jpegfile=NULL,
                           stations=NULL,...){

  if (!is.null(jpegfile)) {
    jpeg(jpegfile, width=800, height=800)
    on.exit(dev.off())
  }

  colors <- colorRampPalette(c('darkblue', 'blue', 'lightblue', 'yellow', 'red', 'darkred'), space = 'Lab')(ncolors)
  cuts <- seq(cmin,cmax,len=ncolors+1)

  r <- raster(g,layer=layer)
  if (!is.null(zoom)) {
    r<-crop(r,zoom)
  }

  layers <-NULL

  if (plot.europe) {
    #    shape <-  readOGR("/Volumes/Elements/data/POSSE/Borders_landsea_Europe/", "Europe", verbose = FALSE)
    shape <- readOGR("./TMP/naturalearthdata/", "ne_10m_admin_0_countries",verbose=FALSE)
    shape <- spTransform(shape,CRSobj = crs(g))

    Borders <- list("sp.polygons", shape, col="black", lwd=1,
                    xlim=c(bbox(shape)[[1]], bbox(shape)[[3]]),
                    ylim=c(bbox(shape)[[2]], bbox(shape)[[4]]),
                    first=FALSE)
    layers <- list(Borders)
  }

  # add station locations
  if (!is.null(stations)) {
    temppoints <- list("sp.points",stations[,"temperature"],pch='.',cex=2,col="black")
    layers[[length(layers)+1]] <- temppoints
  }

  c1<-1.0
  c2<-2.0
  cx<-0.0
  s<-spplot(r,
            col.regions=colors,  at = cuts,
            edge.col = "transparent", pretty=TRUE,
            scales=list(draw = TRUE, cex=c1),
            ylab=list("Latitude", cex=c1),
            xlab=list("Longitude", cex=c1),
            colorkey=list(space="right", cex=c2,at=cuts),
            sp.layout = layers,
            xlim=c(bbox(r)[[1]]-cx, bbox(r)[[3]]+cx),
            ylim=c(bbox(r)[[2]]-cx, bbox(r)[[4]]+cx),
            main = main)

  if (!is.null(jpegfile))
    print(s)
  return(s)
}

# some test routines, that will be removed later

plot_MOS_field3<-function(grid,layer=1,cmin=-35,cmax=35,ncolors=50,plot.europe=FALSE,
                          stations=NULL,zoom=NULL,...) {
  colors <- colorRampPalette(c('darkblue', 'blue', 'lightblue', 'yellow', 'red', 'darkred'), space = 'Lab')(ncolors)
  cuts <- seq(cmin,cmax,len=ncolors)
  r <- raster(grid,layer=layer)
  if (!is.null(zoom)) {
    r<-crop(r,zoom)
  }

  cx<-0
  plot(r,col=colors,breaks=cuts,legend=FALSE,axes=FALSE,
       xlim=c(xmin(r), xmax(r)), ylim=c(ymin(r), ymax(r)),
       ...)
  r.range <- c(cmin,cmax)
  plot(r, legend.only=TRUE, col=colors,
       legend.width = 1, legend.shrink = 0.75,
       axis.args=list(at=seq(r.range[1], r.range[2], by=5),
                      labels=seq(r.range[1], r.range[2], by=5),
                      cex.axis=0.6),
       legend.args=list(text='temperature', side=4, font=2, line=2.5, cex=0.8))
  if(plot.europe){
    # shape <-  readOGR("/Volumes/Elements/data/POSSE/Borders_landsea_Europe/", "Europe", verbose = FALSE)
    # layer(sp.polygons(shape, col="black", lwd=1 ))
    # lines(sp.polygons(shape, col="black", lwd=13 ))
    #    lines(shape, col="black", lwd=1 )
    shape <- readOGR("./TMP/naturalearthdata/", "ne_10m_admin_0_countries",verbose=FALSE)
    shape <- spTransform(shape,CRSobj = crs(grid))
    lines(shape,lwd=0.5)
    #    lines(sp.polygons(shape, col="black", lwd=13 ))
  }

  ## add station locations
  if (!is.null(stations)) {
    points(stations,pch='.',cex=2,col="black")
  }


}


# plot SpatialPixelsDataFrame
plot_MOS_field2 <- function(g,layer=1,main="",cmin=-40,cmax=40,ncolors=50,
                            plot.europe=FALSE,zoom=NULL,jpegfile=NULL,
                            stations=NULL,mapfile=0,...){
  if (!is.null(jpegfile)) {
    jpeg(jpegfile, width=800, height=800)
    on.exit(dev.off())
  }


  colors <- colorRampPalette(c('darkblue', 'blue', 'lightblue', 'yellow', 'red', 'darkred'), space = 'Lab')(ncolors)
  cuts <- seq(cmin,cmax,len=ncolors+1)

  c1<-1.0
  c2<-2.0
  cx<-0.0

  r <- raster(g,layer=layer)
  if (!is.null(zoom)) {
    r<-crop(r,zoom)
  }

  s <- image(r,col=colors,breaks=cuts,main=main,axes=TRUE,
             ylab=list("Latitude", cex=c1),
             xlab=list("Longitude", cex=c1),
             xlim=c(bbox(r)[[1]]-cx, bbox(r)[[3]]+cx),
             ylim=c(bbox(r)[[2]]-cx, bbox(r)[[4]]+cx)
  )

  if (plot.europe) {
    if (mapfile==2) {
      map("world",add=TRUE)
    } else {
      if (mapfile==1) {
        shape <-  readOGR("/Volumes/Elements/data/POSSE/Borders_landsea_Europe/", "Europe", verbose = FALSE)
      } else {
        #        shape <- readOGR("./TMP/naturalearthdata/", "ne_110m_admin_0_countries",verbose=FALSE)
        #        shape <- readOGR("./TMP/naturalearthdata/", "ne_50m_admin_0_countries",verbose=FALSE)
        shape <- readOGR("./TMP/naturalearthdata/", "ne_10m_admin_0_countries",verbose=FALSE)
      }
      shape <- spTransform(shape,CRSobj = crs(g))
      lines(shape,lwd=0.5)
    }
  }

  ## add station locations
  if (!is.null(stations)) {
    points(stations,pch='.',cex=2,col="black")
  }

  ## add legend
  #  legend(xmax(r), ymax(r),
  #          legend = cuts,
  #         col=colors,
  #         fill=colors,
  #         cex=0.9
  #  )

  invisible(s)
}
