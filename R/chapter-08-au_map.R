#' @name au_map
#' @title Australia maps
#' 
#' `[figure8.1()]`: complete Australia map.
#' `[figure8.7()]`: Australia left-bottom subplot.
#' `[figure8.18()]`: Australia with temperature ribbon
NULL

#' @keywords internal
#' @export
figure8.1 <- function() {
    mapLong   <- c(132, 136)
    mapLat    <- c(-35, -31.5)
    imageLong <- range(RGraphics::fluoro.predict$x)
    imageLat  <- range(RGraphics::fluoro.predict$y)
    zbreaks   <- seq(min(RGraphics::fluoro.predict$z, na.rm=TRUE), 
                   max(RGraphics::fluoro.predict$z, na.rm=TRUE), 
                   length=10)
    zcol <- cut(RGraphics::fluoro.predict$z, zbreaks,
                include.lowest=TRUE, labels=FALSE)
    ozgrays   <- gray(0.5 + 1:9/20)
    imageCols <- ozgrays[zcol]
    ozimage   <- ozImage(mapLong, mapLat, 
                       imageLong, imageLat, imageCols)

    ribbonlegend <- ribbonLegend(breaks=zbreaks, 
                                 cols=ozgrays, 
                                 scale=range(zbreaks),
                                 gp=gpar(cex=0.7))

    ozkey <- ozKey(x=unit(1, "npc") - unit(1, "mm"),
                   y=unit(1, "npc") - unit(1, "mm"),
                   width=unit(3.5, "cm"),
                   height=unit(2, "cm"),
                   just=c("right", "top"),
                   mapLong, mapLat)

    grid.rect(gp=gpar(col="gray"))
    fg <- frameGrob()
    fg <- packGrob(fg, ozimage)
    fg <- placeGrob(fg, ozkey)
    fg <- packGrob(fg, ribbonlegend, "right")
    grid.draw(fg)
}

#' @keywords internal
#' @export
figure8.5 <- function() {
    grid.rect(gp=gpar(col="gray"))
    grid.ozFun(oz::ozRegion())
}

# left-bottom subplot
#' @keywords internal
#' @export
figure8.6 <- function() {
    mapLong <- c(132, 136)
    mapLat <- c(-35, -31.5)
    imageLong <- range(RGraphics::fluoro.predict$x)
    imageLat <- range(RGraphics::fluoro.predict$y)
    zbreaks <- seq(min(RGraphics::fluoro.predict$z, na.rm=TRUE), 
                   max(RGraphics::fluoro.predict$z, na.rm=TRUE), 
                   length=10)
    zcol <- cut(RGraphics::fluoro.predict$z, zbreaks,
                include.lowest=TRUE, labels=FALSE)
    ozgrays <- gray(0.5 + 1:9/20)
    imageCols <- ozgrays[zcol]

    grid.rect(gp=gpar(col="gray"))
    grid.ozFun(oz::ozRegion(xlim=mapLong, ylim=mapLat))

    downViewport("ozvp")
    pushViewport(viewport(y=min(imageLat), 
                          x=max(imageLong), 
                          height=abs(diff(imageLat)), 
                          width=abs(diff(imageLong)),
                          default.units="native", 
                          just=c("right", "bottom")))
    grid.imageFun(50, 50, col=imageCols)
    upViewport(0)
}

#' @keywords internal
#' @export
figure8.7 <- function() {
    zbreaks <- seq(min(RGraphics::fluoro.predict$z, na.rm=TRUE), 
                   max(RGraphics::fluoro.predict$z, na.rm=TRUE), 
                   length=10)
    zcol <- cut(RGraphics::fluoro.predict$z, zbreaks,
                include.lowest=TRUE, labels=FALSE)
    ozgrays <- gray(0.5 + 1:9/20)
    fill_new <- rev(ozgrays)[zcol]

    grid.edit("image", gp=gpar(fill=fill_new))
    grid.gedit("^ozlines[0-9]+$", gp=gpar(col="gray", lwd=2))
}

# test ribbonLegend
#' @export
figure8.18 <- function() {
    grid.ozGrob(oz::ozRegion())
    downViewport("ozvp")
    temp <- RGraphics::ozTemp
    for (i in 1:(dim(temp)[1])) {
        grid.points(ozTemp$long[i], ozTemp$lat[i], pch=16)
        rl <- ribbonLegend(breaks=c(min(ozTemp$min), 
                                    ozTemp$min[i], 
                                    ozTemp$max[i], 
                                    max(ozTemp$max)),
                           cols=c("white", "gray", "white"),
                           gp=gpar(cex=.7))
        
        pushViewport(viewport(x=unit(ozTemp$long[i], "native"),
                              y=unit(ozTemp$lat[i], "native"),
                              height=unit(1, "in"),
                              # width=unit(1, "in"),
                              width=grobWidth(rl),
                              clip="off"))
        grid.rect()
        grid.circle(r=1, gp=gpar(col="gray", fill="white", alpha=0.8))
        grid.draw(rl)
        popViewport()
        # 
    }
    upViewport(0)
}
