figure16.1 <- function() {
    tetra <- function() {
        t1 <- rgl::tetrahedron3d()
        t2vb <- t1$vb
        t2vb[1, ] <- -3
        t2 <- rgl::tmesh3d(t2vb, t1$it)
        plane <- rgl::qmesh3d(rbind(rep(-3.01, 4),
                                    c(-2, -2, 2, 2),
                                    c(-3, 3, 3, -3),
                                    rep(1, 4)),
                              matrix(1:4, ncol=1))
        rgl::open3d(windowRect=c(0, 0, 600, 600))
        # clear3d()
        rgl::shade3d(plane, color="white", specular="black")
        rgl::wire3d(plane)
        rgl::wire3d(t1, lwd=3)
        rgl::wire3d(t2, lwd=3)
        rgl::segments3d(rbind(t2$vb[1, t2$it], 
                              t1$vb[1, t1$it]),
                        rbind(t2$vb[2, t2$it], 
                              t1$vb[2, t1$it]),
                        rbind(t2$vb[3, t2$it], 
                              t1$vb[3, t1$it]),
                        col="gray", lwd=3)
        rgl::view3d(40, -30)
    }

    t1 <- rgl::cube3d()
    t1tube <- t1
    t1tube$ib <- t1tube$ib[, -(3:4)]
    t2vb <- t1$vb
    t2vb[1, ] <- -5
    t2 <- rgl::qmesh3d(t2vb, t1$ib)
    plane <- rgl::qmesh3d(rbind(rep(-5.01, 4),
                                c(-2, -2, 2, 2),
                                c(-3, 3, 3, -3),
                                rep(1, 4)),
                          matrix(1:4, ncol=1))
    rgl::open3d(windowRect=c(0, 0, 600, 600))
    # clear3d()
    rgl::shade3d(plane, color="white", 
                 ambient="white", specular="white", emission="white")
    rgl::wire3d(plane)
    rgl::shade3d(t1tube, color="white", specular="black")
    rgl::wire3d(t1, lwd=3)
    rgl::wire3d(t2, lwd=3)
    rgl::segments3d(rbind(t2$vb[1, t2$ib[, 4]], 
                          t1$vb[1, t1$ib[, 4]]),
                    rbind(t2$vb[2, t2$ib[, 4]], 
                          t1$vb[2, t1$ib[, 4]]),
                    rbind(t2$vb[3, t2$ib[, 4]], 
                          t1$vb[3, t1$ib[, 4]]),
                    col="gray", lwd=3)
    rgl::view3d(55, -20, fov=0)
    rgl::rgl.postscript("Figures/threed-3dproj.eps")
    system("epstopdf Figures/threed-3dproj.eps")
    system("convert Figures/threed-3dproj.pdf Web/threed-3dproj.png")
}

figure16.2 <- function() {
    tetra <- function() {
        t2vb <- t1$vb
        t2vb[1, ] <- -3
        t2vb[2, c(1, 4)] <- t2vb[2, c(1, 4)]*.8
        t2vb[3, c(1, 4)] <- t2vb[3, c(1, 4)]*.8
        t2vb[2, 2:3] <- t2vb[2, 2:3]*.6
        t2vb[3, 2:3] <- t2vb[3, 2:3]*.6
        t2 <- rgl::tmesh3d(t2vb, t1$it)
        t3vb <- t1$vb
        t3vb[1, ] <- -10
        t3vb[2, ] <- 0
        t3vb[3, ] <- 0
        t3 <- rgl::tmesh3d(t3vb, t1$it)
        rgl::open3d(windowRect=c(0, 0, 600, 600))
        # clear3d()
        rgl::shade3d(plane, color="white", specular="black")
        rgl::wire3d(plane)
        rgl::wire3d(t1, lwd=3)
        rgl::wire3d(t2, lwd=3)
        rgl::segments3d(rbind(t3$vb[1, t3$it], 
                              t1$vb[1, t1$it]),
                        rbind(t3$vb[2, t3$it], 
                              t1$vb[2, t1$it]),
                        rbind(t3$vb[3, t3$it], 
                              t1$vb[3, t1$it]),
                        col="gray", lwd=3)
        rgl::shade3d(rgl::translate3d(rgl::scale3d(rgl::cube3d(), .1, .1, .1), -10, 0, 0))
        rgl::view3d(50, -20, zoom=.8)
    }

    t1 <- rgl::cube3d()
    t1tube <- t1
    t1tube$ib <- t1tube$ib[, -(3:4)]
    t2vb <- t1$vb
    t2vb[2, t1$ib[, 4]] <- t2vb[2, t1$ib[, 4]]*.4
    t2vb[3, t1$ib[, 4]] <- t2vb[3, t1$ib[, 4]]*.4
    t2vb[2, t1$ib[, 3]] <- t2vb[2, t1$ib[, 3]]*.6
    t2vb[3, t1$ib[, 3]] <- t2vb[3, t1$ib[, 3]]*.6
    t2vb[1, ] <- -4.5
    t2 <- rgl::qmesh3d(t2vb, t1$ib)
    t2tube <- t2
    t2tube$ib <- t2tube$ib[, -(3:4)]
    t3 <- rgl::translate3d(rgl::scale3d(rgl::cube3d(), .1, .1, .1), -10, 0, 0)
    plane <- rgl::qmesh3d(rbind(rep(-4.51, 4),
                                c(-2, -2, 2, 2),
                                c(-3, 3, 3, -3),
                                rep(1, 4)),
                          matrix(1:4, ncol=1))
    rgl::open3d(windowRect=c(0, 0, 600, 300))
    # clear3d()
    rgl::shade3d(plane, color="white", 
                 ambient="white", specular="white", emission="white")
    rgl::wire3d(plane)
    rgl::shade3d(t1tube, color="white", specular="black")
    rgl::wire3d(t1, lwd=3)
    rgl::shade3d(t2tube, color="white", specular="black")
    rgl::wire3d(t2, lwd=3)
    rgl::segments3d(rbind(-10, 
                          t1$vb[1, t1$ib[, 4]]),
                    rbind(0, 
                          t1$vb[2, t1$ib[, 4]]),
                    rbind(0,
                          t1$vb[3, t1$ib[, 4]]),
                    col="gray", lwd=3)
    rgl::shade3d(t3)
    rgl::view3d(50, -15, fov=0, zoom=.7)
    rgl::rgl.postscript("Figures/threed-3dvp.eps")
    system("epstopdf Figures/threed-3dvp.eps")
    system("convert Figures/threed-3dvp.pdf Web/threed-3dvp.png")
}

figure16.3 <- function() {
    quakes <- read.csv("Quake/earthquakes.csv")
    NZquakes <- quakes[c("LAT", "LONG", "MAG", "DEPTH")]

    cantyQuakes <- quakes[quakes$LAT < -42.4 & quakes$LAT > -44 & 
                              quakes$LONG > 171 & quakes$LONG < 173.5, ]

    quakeDens <- MASS::kde2d(cantyQuakes$LONG, cantyQuakes$LAT, n=30)

    par(mar=rep(0, 4))
    persp(quakeDens)
}

figure16.4 <- function() {
    quakes <- read.csv("Quake/earthquakes.csv")
    NZquakes <- quakes[c("LAT", "LONG", "MAG", "DEPTH")]

    cantyQuakes <- quakes[quakes$LAT < -42.4 & quakes$LAT > -44 & 
                              quakes$LONG > 171 & quakes$LONG < 173.5, ]

    quakeDens <- MASS::kde2d(cantyQuakes$LONG, cantyQuakes$LAT, n=30)

    par(mar=rep(0, 4))
    persp(quakeDens, scale=FALSE, expand=0.02,
          theta=60, d=.1, r=.1,
          xlab="longitude", ylab="latitude", zlab="")
}

figure16.5 <- function() {
    quakes <- read.csv("Quake/earthquakes.csv")
    NZquakes <- quakes[c("LAT", "LONG", "MAG", "DEPTH")]

    cantyQuakes <- quakes[quakes$LAT < -42.4 & quakes$LAT > -44 & 
                              quakes$LONG > 171 & quakes$LONG < 173.5, ]

    quakeDens <- MASS::kde2d(cantyQuakes$LONG, cantyQuakes$LAT, n=30)

    par(mar=rep(0, 4))
    zinterp <- with(quakeDens,
                    z[-1, -1] + z[-1, -ncol(z)] + 
                        z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])
    persp(quakeDens, scale=FALSE, expand=0.02,
          theta=60, d=.1, r=.1, axes=FALSE, box=FALSE,
          col=gray(.4 + 1:6/10)[cut(zinterp, 6)])
}

figure16.6 <- function() {
    quakes <- read.csv("Quake/earthquakes.csv")
    NZquakes <- quakes[c("LAT", "LONG", "MAG", "DEPTH")]

    cantyQuakes <- quakes[quakes$LAT < -42.4 & quakes$LAT > -44 & 
                              quakes$LONG > 171 & quakes$LONG < 173.5, ]

    quakeDens <- MASS::kde2d(cantyQuakes$LONG, cantyQuakes$LAT, n=30)

    shallowCantyQuakes <- subset(cantyQuakes, DEPTH < 20)

    trellis.device("pdf", color=FALSE,
                   file="Figures/threed-cloud%d.pdf", onefile=FALSE)
    for (i in seq(40, 80, 20)) {
        print(cloud(-DEPTH ~ LONG + LAT, shallowCantyQuakes,
                    xlim=c(171, 173), ylim=c(-44.5, -42.5),
                    pch=16, col=rgb(0, 0, 0, .5),
                    screen=list(z=i, x=-70)))
    }

    dev.off()
}

figure16.7 <- function() {
    quakes <- read.csv("Quake/earthquakes.csv")
    NZquakes <- quakes[c("LAT", "LONG", "MAG", "DEPTH")]

    cantyQuakes <- quakes[quakes$LAT < -42.4 & quakes$LAT > -44 & 
                              quakes$LONG > 171 & quakes$LONG < 173.5, ]

    quakeDens <- MASS::kde2d(cantyQuakes$LONG, cantyQuakes$LAT, n=30)

    shallowCantyQuakes <- subset(cantyQuakes, DEPTH < 20)

    par(lab=c(3, 3, 0))
    s3d <- with(shallowCantyQuakes,
                scatterplot3d(-DEPTH ~ LONG + LAT,
                              angle=30, scale.y=0.45, type="n",
                              pch=16, color=rgb(0, 0, 0, .5),
                              x.ticklabs=pretty(LONG, 3),
                              grid=FALSE, zlim=c(-20, 0)))

    quakeDensXY <- MASS::kde2d(shallowCantyQuakes$LONG, 
                               shallowCantyQuakes$LAT, n=30)
    lapply(contourLines(quakeDensXY, nlevels=8),
           function(cl) {
               polygon(s3d$xyz.convert(cl$x, cl$y, 
                                       rep(-20, length(cl$x))),
                       lwd=.5, col=gray(.8 - cl$level/20),
                       border=NA)
           })

    quakeDensXZ <- MASS::kde2d(shallowCantyQuakes$LONG, 
                               -shallowCantyQuakes$DEPTH, n=30)
    lapply(contourLines(quakeDensXZ, nlevels=8),
           function(cl) {
               polygon(s3d$xyz.convert(cl$x, 
                                       rep(-43.2, length(cl$x)),
                                       cl$y),
                       lwd=.5, col=gray(.8 - 2*cl$level),
                       border=NA)
           })
    quakeDensYZ <- MASS::kde2d(shallowCantyQuakes$LAT, 
                               -shallowCantyQuakes$DEPTH, n=30)
    lapply(contourLines(quakeDensYZ, nlevels=8),
           function(cl) {
               polygon(s3d$xyz.convert(rep(171.5, length(cl$x)),
                                       cl$x, cl$y),
                       lwd=.5, col=gray(.8 - cl$level),
                       border=NA)
           })
    lapply(contourLines(quakeDensYZ, nlevels=8),
           function(cl) {
               polygon(s3d$xyz.convert(rep(173.5, length(cl$x)),
                                       cl$x, cl$y),
                       lwd=.5, col=gray(.8 - cl$level),
                       border=NA)
           })

    with(shallowCantyQuakes,
         s3d$points3d(-DEPTH ~ LONG + LAT, pch=16,
                      col=rgb(0, 0, 0, .3)))
    s3d$box()
}

figure16.8 <- function() {
    quakes <- read.csv("Quake/earthquakes.csv")
    NZquakes <- quakes[c("LAT", "LONG", "MAG", "DEPTH")]

    cantyQuakes <- quakes[quakes$LAT < -42.4 & quakes$LAT > -44 & 
                              quakes$LONG > 171 & quakes$LONG < 173.5, ]

    quakeDens <- MASS::kde2d(cantyQuakes$LONG, cantyQuakes$LAT, n=30)

    rgl::open3d(windowRect=c(0, 0, 900, 450))
    # clear3d("all")
    rgl::persp3d(quakeDens$x, quakeDens$y, quakeDens$z, 
                 aspect=c(1, 0.55, .2), col="white", box=FALSE,
                 axes=FALSE, xlab="", ylab="", zlab="")

    rgl::par3d(userMatrix=rgl::rotationMatrix(-80/180*pi, 1, 0, 0)%*%
                   rgl::rotationMatrix(-65/180*pi, 0, 0, 1),
               zoom=.5)
    rgl::snapshot3d("Figures/threed-rglpersp.png")
    system("cp Figures/threed-rglpersp.png Web/")
}

figure16.9 <- function() {
    quakes <- read.csv("Quake/earthquakes.csv")
    NZquakes <- quakes[c("LAT", "LONG", "MAG", "DEPTH")]

    cantyQuakes <- quakes[quakes$LAT < -42.4 & quakes$LAT > -44 & 
                              quakes$LONG > 171 & quakes$LONG < 173.5, ]

    quakeDens <- MASS::kde2d(cantyQuakes$LONG, cantyQuakes$LAT, n=30)

    shallowCantyQuakes <- subset(cantyQuakes, DEPTH < 20)

    d <- with(shallowCantyQuakes, 
              {
                  misc3d::kde3d(LONG, LAT, -DEPTH, 
                                h=c(.1, .1, 2), n = 30)
              })

    rgl::open3d(windowRect=c(0, 0, 900, 900))
    with(shallowCantyQuakes, 
         {
             rgl::plot3d(LONG, LAT, -DEPTH, 
                         aspect=c(1, 0.55, 1), 
                         axes=TRUE, box=FALSE,
                         xlab="", ylab="", zlab="")
             misc3d::contour3d(d$d, c(.4, .1), d$x, d$y, d$z,
                               color=rep("gray80", 2), 
                               color2="gray", specular="black",
                               engine="rgl", add=TRUE, alpha=.5)
         })

    rgl::par3d(userMatrix=rgl::rotationMatrix(-60/180*pi, 1, 0, 0)%*%
                   rgl::rotationMatrix(-40/180*pi, 0, 0, 1),
               zoom=.9)
    rgl::snapshot3d("Figures/contour3d.png")
    system("cp Figures/contour3d.png Web/threed-contour3d.png")
}

figure16.10 <- function() {
    rgl::open3d(windowRect=c(0, 0, 600, 600))
    rgl::clear3d("all")
    rgl::light3d()
    rgl::material3d(shininess=100, specular="black")
    # Head
    radius <- function(d) {
        pchisq(d^2, 3)
    }
    rgl::shade3d(rgl::ellipse3d(diag(3), level=radius(1),
                                centre=c(0, 0, 1)),
                 color="yellow")
    # Neck
    # logo is 100x76
    png("rlogoExtended.png",
        width=500, height=250)
    grid.rect(gp=gpar(col=NA, fill="yellow"))

    rlogo <- as.raster(png::readPNG(system.file("extra", "Rlogo.png", 
                                                package="RGraphics")))
    rlogo[rlogo == "#FFFFFF"] <- "yellow"
    grid.raster(rlogo, x=.6, y=.01, width=.08, just=c("bottom"))
    dev.off()
    rgl::shade3d(rgl::cylinder3d(cbind(0, 0, c(-1.4, 1)),
                                 e1=cbind(0, 0, 1),
                                 e2=cbind(1, 0, 0),
                                 sides=100),
                 color="yellow",
                 texture="rlogoExtended.png",
                 texcoords=list(x=rep(seq(1, 0, length.out=101), each=4)[-c(1:2, 403:404)],
                                y=rep(c(0, 1, 1, 0), 50)))
    old <- function() {
        rgl::shade3d(rgl::cylinder3d(cbind(0, 0, c(-1.3, 1)),
                                     e1=cbind(0, 0, 1),
                                     e2=cbind(1, 0, 0),
                                     sides=100),
                     color="yellow")
    }
    # Eyes
    eyeball <- rgl::ellipse3d(diag(3), level=radius(.4))
    rgl::shade3d(rgl::translate3d(eyeball, .8, .35, .7),
                 color="white")
    rgl::shade3d(rgl::translate3d(eyeball, .8, -.35, .7),
                 color="white")
    # Translate radius of eye, rotate, translate position of eye
    pupil <- rgl::rotate3d(rgl::translate3d(rgl::ellipse3d(diag(3),
                                                           level=radius(.05)),
                                            .4, 0, 0),
                           30/180*pi, 0, 0, 1)
    rgl::shade3d(rgl::translate3d(pupil, .8, .35, .7),
                 color="black")
    rgl::shade3d(rgl::translate3d(pupil, .8, -.35, .7),
                 color="black")
    # points3d(1.21, c(-.35, .35), .7, cex=3)
    # Nose
    rgl::shade3d(rgl::cylinder3d(cbind(c(1, 1.3), 0, .3),
                                 radius=.2,
                                 e1=cbind(1, 0, 0),
                                 e2=cbind(0, 1, 0),
                                 sides=100),
                 color="yellow")
    rgl::shade3d(rgl::ellipse3d(diag(3), level=radius(.2),
                                centre=c(1.3, 0, .3)),
                 color="yellow")
    # Mouth
    rgl::shade3d(rgl::ellipse3d(diag(3), level=radius(.8),
                                centre=c(.6, 0, -.5)),
                 color="tan")
    angle <- seq(-65, 65, length=30)/180*pi
    rgl::lines3d(.6 + .81*cos(angle), .81*sin(angle), -.5, lwd=3)
    # Hair on top
    angle <- seq(15, 165, length=30)/180*pi
    rgl::lines3d(.2, .7*cos(angle), 1.5 + .7*sin(angle), lwd=3)
    rgl::lines3d(-.2, .7*cos(angle), 1.5 + .7*sin(angle), lwd=3)
    # Hair on sides
    rgl::lines3d(seq(.5, -.5, length=5), -1, rep(c(.3, .8), length=5), lwd=3)
    rgl::lines3d(seq(.5, -.5, length=5), 1, rep(c(.3, .8), length=5), lwd=3)

    rgl::par3d(userMatrix=rgl::rotationMatrix(-pi/2, 1, 0, 0)%*%
                   rgl::rotationMatrix(-50/180*pi, 0, 0, 1)%*%
                   rgl::rotationMatrix(10/180*pi, 1, 0, 0))

    rgl::snapshot3d("homer.png")
}

figure16.11 <- function() {
    quakes <- read.csv("Quake/earthquakes.csv")
    NZquakes <- quakes[c("LAT", "LONG", "MAG", "DEPTH")]

    cantyQuakes <- quakes[quakes$LAT < -42.4 & quakes$LAT > -44 & 
                              quakes$LONG > 171 & quakes$LONG < 173.5, ]

    quakeDens <- MASS::kde2d(cantyQuakes$LONG, cantyQuakes$LAT, n=30)

    shallowCantyQuakes <- subset(cantyQuakes, DEPTH < 20)

    with(shallowCantyQuakes,
         vrmlgen::cloud3d(LONG, LAT, -DEPTH,
                          filename="vrmlgen.wrl",
                          cols="white"))
}
