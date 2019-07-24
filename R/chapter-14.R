
figure14.1 <- function() {
    colorado <- maptools::readShapeSpatial(system.file("extra", "10m-colorado.shp",
                                                       package="RGraphics"))
    par(mar=rep(0, 4))
    sp::plot(colorado, col="gray")
}

figure14.2 <- function() {
    par(mar=rep(0, 4))
    maps::map(regions="Brazil", fill=TRUE, col="gray")

    box("figure", col="gray", lwd=2)
}

figure14.3 <- function() {
    brazil <- 
        maptools::readShapeSpatial(system.file("extra", "10m-brazil.shp",
                                               package="RGraphics"))

    par(mar=rep(0, 4))
    # Need to spec. sp:: here so that code works in 'RGraphics' package
    sp::plot(brazil, col="gray")
    box("figure", col="gray", lwd=2)
}

figure14.4 <- function() {
    brazil <- 
        maptools::readShapeSpatial(system.file("extra", "10m-brazil.shp",
                                               package="RGraphics"))

    print(
        sp::spplot(brazil, "Regions", col.regions=gray(5:1/6))
    )
}

figure14.5 <- function() {
    brazil <- 
        maptools::readShapeSpatial(system.file("extra", "10m-brazil.shp",
                                               package="RGraphics"))

    brazilRegions <- 
        maptools::readShapeSpatial(system.file("extra", 
                                               "10m_brazil_regions.shp",
                                               package="RGraphics"))

    brazilCapitals <- 
        maptools::readShapeSpatial(system.file("extra",
                                               "10m_brazil_capitals.shp",
                                               package="RGraphics"))

    print(
        sp::spplot(brazil, "Regions", 
                   col.regions=gray.colors(5, 0.8, 0.3),
                   col="white", 
                   panel=function(...) {
                       sp::panel.polygonsplot(...)
                       sp::sp.lines(brazilRegions, col="gray40")
                       labels <- brazilCapitals$Name
                       w <- stringWidth(labels)
                       h <- stringHeight(labels)
                       locs <- sp::coordinates(brazilCapitals)
                       grid.rect(unit(locs[, 1], "native"),
                                 unit(locs[, 2], "native"),
                                 w, h, just=c("right", "top"),
                                 gp=gpar(col=NA, fill=rgb(1, 1, 1, .5)))
                       sp::sp.text(locs, labels, adj=c(1, 1))
                       sp::sp.points(brazilCapitals, pch=21,
                                     col="black", fill="white")
                   })
    )
}

figure14.6 <- function() {
    marajo <- 
        maptools::readShapeSpatial(system.file("extra", "marajo.shp",
                                               package="RGraphics"))
    par(mar=rep(0, 4))
    sp::plot(marajo, col="gray", pbg="white")
}

figure14.7 <- function() {
    iceland <- 
        maptools::readShapeSpatial(system.file("extra", "10m-iceland.shp",
                                               package="RGraphics"))
    par(mar=rep(0, 4))
    sp::plot(iceland, col="gray")
}

figure14.8 <- function() {
    iceland <- 
        maptools::readShapeSpatial(system.file("extra", "10m-iceland.shp",
                                               package="RGraphics"))

    sp::proj4string(iceland) <- sp::CRS("+proj=longlat +ellps=WGS84")

    par(mar=rep(0, 4))
    sp::plot(iceland, col="gray")

}

figure14.9 <- function() {
    iceland <- 
        maptools::readShapeSpatial(system.file("extra", "10m-iceland.shp",
                                               package="RGraphics"))

    sp::proj4string(iceland) <- sp::CRS("+proj=longlat +ellps=WGS84")

    icelandMercator <- sp::spTransform(iceland, 
                                       sp::CRS("+proj=merc +ellps=GRS80"))

    par(mar=rep(0, 4))
    sp::plot(iceland, col="gray80", border="white", lwd=3)
    par(new=TRUE)
    sp::plot(icelandMercator)
}

figure14.10 <- function() {
    brazil <- 
        maptools::readShapeSpatial(system.file("extra", "10m-brazil.shp",
                                               package="RGraphics"))

    sp::proj4string(brazil) <- sp::CRS("+proj=longlat +ellps=WGS84")

    glines <- sp::gridlines(brazil)
    glinesOrtho <- sp::spTransform(glines, sp::CRS("+proj=ortho"))
    par(mar=rep(0, 4))
    brazilOrtho <- sp::spTransform(brazil, sp::CRS("+proj=ortho"))
    sp::plot(brazilOrtho, col="gray")
    sp::plot(glinesOrtho, lty="dashed", add=TRUE)

}

figure14.11 <- function() {
    brazil <- 
        maptools::readShapeSpatial(system.file("extra", "10m-brazil.shp",
                                               package="RGraphics"))

    # Read in prepared raster
    brazilRelief <- raster::raster(system.file("extra", "brazilRelief.tif",
                                               package="RGraphics"))

    # Make PNG version for this one because otherwise it's TOO big
    png("Figures/maps-brazilraster.png",
        width=900, height=900)
    par(mar=rep(0, 4))
    raster::image(brazilRelief, col=gray(0:255/255), maxpixels=1e6)
    sp::plot(brazil, add=TRUE)
    box(lwd=4)
    dev.off()
    system("cp Figures/maps-brazilraster.png Web/")
}
