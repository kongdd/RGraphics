figure18.1 <- function() {

}

figure18.2 <- function() {
    source(system.file("extra", "as.raster.R", package="RGraphics"))

    moonPhase <- function(x, y, phase, size=.05) {
        # size is in inches
        n <- 17
        angle <- seq(0, 2*pi, length=n)
        xx <- x + cos(angle)*xinch(size)
        yy <- y + sin(angle)*yinch(size)
        if (phase == "New")
            fill <- "black"
        else
            fill <- "white"
        polygon(xx, yy, col=fill)
        if (phase == "1Q")
            polygon(xx[(n/4):(n*3/4) + 1],
                    yy[(n/4):(n*3/4) + 1],
                    col="black")
        if (phase == "3Q")
            polygon(xx[c(1:(n/4 + 1), (n*3/4 + 1):n)],
                    yy[c(1:(n/4 + 1), (n*3/4 + 1):n)],
                    col="black")
    }

    # Original image from NASA
    # http://grin.hq.nasa.gov/ABSTRACTS/GPN-2000-000473.html
    rasterMoon <- pixmap::read.pnm(system.file("extra", "GPN-2000-000473.pgm",
                                               package="RGraphics"))
    par(pin=c(3.5, 1.75), oma=c(0, 3, 0, 0), xaxs="i", yaxs="i", cex=.7)
    plot.new()
    rect(0, 0, 1, 1, col="black")
    rasterImage(rasterMoon, .25, 0, .75, 1*813/703)
    par(new=TRUE, xaxs="r", yaxs="r", las=1)
    plot(lowTideDate, lowTideHour, type="n",
         ylim=range(mainHours), axes=FALSE, ann=FALSE)
    # dashed reference lines
    abline(v=phases$date,
           col="white", lty="dashed")
    for (subset in list(1:13, 14:29, 30:31)) {
        lines(lowTideDate[subset], lowTideHour[subset],
              lwd=2, col="white")
        points(lowTideDate[subset], lowTideHour[subset],
               pch=16, col="white")
    }
    box()
    axis.POSIXct(1, lowTideDate)
    axis.POSIXct(2, at=mainHours, format="%H:%M")
    mtext("Time of Low Tide (NZDT)", side=2, line=4, las=0, cex=.7)
    mtext("Auckland, New Zealand January 2010", side=1, line=3, cex=.7)
    axis(3, at=phases$date, labels=FALSE)
    par(xpd=NA)
    ymax <- par("usr")[4]
    for (i in 1:nrow(phases))
        moonPhase(phases$date[i], ymax + yinch(.2), 
                  phases$phase[i])
    mtext("Phases of the Moon", side=3, line=3, cex=.7)

    grid.moonPhase <- function(x, y, phase, size=unit(.05, "in")) {
        n <- 17
        angle <- seq(0, 2*pi, length=n)
        xx <- x + cos(angle)*size
        yy <- y + sin(angle)*size
        if (phase == "New")
            fill <- "black"
        else
            fill <- "white"
        grid.polygon(xx, yy, gp=gpar(fill=fill))
        if (phase == "1Q")
            grid.polygon(xx[(n/4):(n*3/4) + 1],
                         yy[(n/4):(n*3/4) + 1],
                         gp=gpar(fill="black"))
        if (phase == "3Q")
            grid.polygon(xx[c(1:(n/4 + 1), (n*3/4 + 1):n)],
                         yy[c(1:(n/4 + 1), (n*3/4 + 1):n)],
                         gp=gpar(fill="black"))
    }

    # grid.newpage()
    pushViewport(viewport(gp=gpar(cex=0.7)),
                 plotViewport(c(4, 5, 3, 1)),
                 dataViewport(as.numeric(lowTideDate), 
                              as.numeric(mainHours)))
    vectorMoon <- 
        grImport::readPicture(system.file("extra", "comic_moon.ps.xml",
                                          package="RGraphics"))
    grImport::grid.picture(vectorMoon)
    grid.segments(unit(phases$date, "native"), 0,
                  unit(phases$date, "native"), 1,
                  gp=gpar(lty="dashed"))
    for (subset in list(1:13, 14:29, 30:31)) {
        grid.lines(lowTideDate[subset], lowTideHour[subset],
                   default.units="native", 
                   gp=gpar(lwd=2))
        grid.points(lowTideDate[subset], lowTideHour[subset],
                    pch=16, size=unit(2, "mm"))
    }
    grid.rect(gp=gpar(fill=NA))
    xTicks <- seq(min(lowTideDate), max(lowTideDate), by="week")
    grid.xaxis(at=xTicks, label=format(xTicks, "%b %d"))
    grid.yaxis(at=mainHours, label=format(mainHours, "%H:%M"))
    grid.text("Time of Low Tide (NZDT)", 
              x=unit(-4, "lines"), rot=90)
    grid.text("Auckland, New Zealand January 2010", 
              y=unit(-3, "lines"))
    grid.xaxis(main=FALSE, at=phases$date, label=FALSE)
    for (i in 1:nrow(phases))
        grid.moonPhase(unit(phases$date[i], "native"),
                       unit(1, "npc") + unit(1, "lines"), 
                       phases$phase[i])
    grid.text("Phases of the Moon", 
              y=unit(1, "npc") + unit(2, "lines"))
    popViewport(2)
}

figure18.3 <- function() {
    source(system.file("extra", "as.raster.R", package="RGraphics"))

    moon <- pixmap::read.pnm(system.file("extra", "GPN-2000-000473.pgm",
                                         package="RGraphics"))
    helmet <- pixmap::read.pnm(system.file("extra", "astronaut.pgm",
                                           package="RGraphics"))

    moonMatrix <- as.matrix(as.raster(moon))
    helmetMatrix <- as.matrix(as.raster(helmet))

    moonCrop <- moonMatrix[120:(119 + nrow(helmetMatrix)),
                           10:(9 + ncol(helmetMatrix))]
    moonGreys <- col2rgb(moonCrop)[1, ]
    helmetRGB <- col2rgb(helmetMatrix)
    helmetMask <- matrix(rgb(helmetRGB[1, ],
                             helmetRGB[2, ],
                             helmetRGB[3, ],
                             moonGreys, maxColorValue=255), ncol=ncol(helmetMatrix))

    pushViewport(viewport(layout=grid.layout(1, 2, respect=TRUE)))
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1),
                 viewport(width=.8, height=.8))
    grid.raster(helmet)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=2),
                 viewport(width=.8, height=.8))
    grid.raster(moonCrop)
    popViewport(2)
}

figure18.4 <- function() {

    source(system.file("extra", "as.raster.R", package="RGraphics"))

    moon <- pixmap::read.pnm(system.file("extra", "GPN-2000-000473.pgm",
                                         package="RGraphics"))
    helmet <- pixmap::read.pnm(system.file("extra", "astronaut.pgm",
                                           package="RGraphics"))

    moonMatrix <- as.matrix(as.raster(moon))
    helmetMatrix <- as.matrix(as.raster(helmet))

    moonCrop <- moonMatrix[120:(119 + nrow(helmetMatrix)),
                           10:(9 + ncol(helmetMatrix))]
    moonGreys <- col2rgb(moonCrop)[1, ]
    helmetRGB <- col2rgb(helmetMatrix)
    helmetMask <- matrix(rgb(helmetRGB[1, ],
                             helmetRGB[2, ],
                             helmetRGB[3, ],
                             moonGreys, maxColorValue=255), ncol=ncol(helmetMatrix))

    grid.rect(width=.99, height=.99)
    grid.raster(helmetMask)
}

figure18.5 <- function() {
    grImport::PostScriptTrace(system.file("extra", "comic_moon.ps",
                                          package="RGraphics"))

    vectorMoon <- grImport::readPicture("comic_moon.ps.xml")

    grImport::picturePaths(vectorMoon[1:6], fill="white", 
                           freeScales=TRUE, nr=2, nc=3)
}

figure18.6 <- function() {
    grImport::PostScriptTrace(system.file("extra", "comic_moon.ps",
                                          package="RGraphics"))

    vectorMoon <- grImport::readPicture("comic_moon.ps.xml")

    grImport::grid.picture(vectorMoon[1:4])
    grImport::grid.picture(vectorMoon, use.gc=FALSE)
}
