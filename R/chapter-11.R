figure11.1 <- function() {
    par(mar=rep(1, 4))
    par(mfrow=c(1, 2))
    plot(faithful)
    gplots::textplot(capture.output(summary(faithful)))
}

figure11.2 <- function() {
    par(mar=rep(1, 4))
    plot(pressure)
    plotrix::addtable2plot(0, 300, pressure[13:19, ])
}

figure11.3 <- function() {
    gridExtra::grid.table(pressure[13:19, ], show.box=TRUE, 
                          separator="black")
}

figure11.4 <- function() {
    par(mar=rep(1, 4))
    x <- rnorm(20)
    y <- rnorm(20)
    plot(x, y, pch=16, col="gray")

    xy <- plotrix::emptyspace(x, y)
    text(xy, label="largest\nempty\nregion")

    xy2 <- Hmisc::largest.empty(x, y, 1, 1)
    rect(xy2$x - .5, xy2$y - .5, 
         xy2$x + .5, xy2$y + .5)
}

figure11.5 <- function() {
    x <- runif(10)
    y <- rnorm(10)

    par(mar=c(1, 1, 2, 1))
    plot(x, y, pch=21, bg="gray", ylim=c(-3, 3), asp=1)
    plotrix::spread.labels(x, y, labels=1:10)

    mtext("spread.labels", side=3, line=0)

    par(mar=c(1, 1, 2, 1))
    plot(x, y, pch=21, bg="gray",  
         ylim=c(-2, 3), xlim=c(-.5, 1.5))
    plotrix::thigmophobe.labels(x, y, labels=1:10)

    mtext("thigmophobe.labels", side=3, line=0)

    par(mar=c(1, 1, 2, 1))
    plot(x, y, pch=21, bg="gray", ylim=c(-3, 3), asp=1)
    adjy <- TeachingDemos::spread.labs(y, strheight("10", cex=1.5))
    text(-0.5, adjy, labels=1:10, pos=2)
    segments(-0.5, adjy, x, y)

    mtext("spread.labs", side=3, line=0)

    par(mar=c(1, 1, 2, 1))
    plot(x, y, pch=16, col="gray", ylim=c(-2, 3), xlim=c(-.5, 1.5))
    maptools::pointLabel(x, y, labels=as.character(1:10))

    mtext("pointLabel", side=3, line=0)

    par(mar=c(1, 1, 2, 1))
    sx <- sort(x)
    sy <- sort(y)
    lines <- list(A=list(x=sx, y=y, lty=1), 
                  B=list(x=sx, y=sy, lty=2),
                  C=list(x=sx, y=rev(y), lty=3), 
                  D=list(x=sx, y=rev(sy), lty=4))

    plot(x, y, type="n", ylim=c(-3, 3))
    lapply(lines, function(l) do.call("lines", l))
    Hmisc::labcurve(lines)

    mtext("labcurve", side=3, line=0)
}

figure11.6 <- function() {
    par(mar=rep(0, 4))
    plot.new()
    plot.window(0:1, c(.1, 1))
    plotrix::draw.circle(.1, .9, radius=1:5/100)
    plotrix::draw.arc(.3, .9, radius=1:5/100, 
                      deg1=45, deg2=seq(360, 160, -50))
    plotrix::arctext("arctext", center=c(.5, .85), radius=.05,
                     stretch=1.2)

    text(.1, .8, "draw.circle")
    text(.3, .8, "draw.arc")
    plotrix::boxed.labels(.7, .85, "boxed.labels", bg="gray90")
    plotrix::textbox(c(.85, 1), .9, "this is a textbox .")

    plotrix::gradient.rect(.05, .5, .15, .7, col=gray(0:20/21))
    plotrix::cylindrect(.25, .5, .35, .7, "black")
    plotrix::rectFill(.45, .5, .55, .7, pch=16)

    text(.1, .45, "gradient.rect")
    text(.3, .45, "cylindrect")
    text(.5, .45, "rectFill")
    x <- c(.65, .65, .75, .75)
    y <- c(.5, .7, .7, .5)
    plotrix::polygon.shadow(x, y, offset=c(2/100, -2/100))
    polygon(x, y, col="white")

    text(.7, .45, "polygon.shadow")
    TeachingDemos::shadowtext(.9, .6, "shadowtext")

    TeachingDemos::my.symbols(seq(.3, .7, .2), .3,
                              TeachingDemos::ms.male, inches=.2)
    TeachingDemos::my.symbols(c(.4, .6), .3,
                              TeachingDemos::ms.female, inches=.2)

    text(.5, .2, "my.symbols")
    box(col="gray")
}

figure11.7 <- function() {
    # library(gridExtra)
    # gridExtra::grid.ellipse(x=1:6/7, y=rep(.8, 6), size=.1, 
    #              default.units="npc", size.unit="npc", 
    #              ar=1:6, angle=1:6*15/180*pi)
    # grid.text("grid.ellipse", y=.7)
    # gridExtra::grid.pattern(x=1:6/7, y=.5, width=unit(.1, "npc"),
    #              height=unit(.1, "npc"), pattern=1:6,
    #              motif.cex=.7, gp=gpar(fill="gray80"))
    # 
    # grid.text("grid.pattern", y=.4)
    # gridExtra::grid.barbed(1:6/7, y=rep(c(.15, .25), 3), 
    #             size=unit(.05, "snpc"), 
    #             pch=21, gp=gpar(fill="gray"))
    # 
    # grid.text("grid.barbed", y=.1)
    # grid.rect(gp=gpar(col="gray", fill=NA))
}

figure11.8 <- function() {
    gplots::plotmeans(mpg ~ cyl, mtcars, 
                      barcol="black", n.label=FALSE, connect=FALSE)
}

figure11.9 <- function() {
    grid.rect(1:10/11, .75, width=1/15, height=1/3,
              gp=gpar(col=NA,
                      fill=colorspace::sequential_hcl(10, 0, 0, c(20, 90))))

    grid.rect(1:10/11, .25, width=1/15, height=1/3,
              gp=gpar(col=NA,
                      fill=colorspace::diverge_hcl(10, 0, 0, c(20, 90))))

    grid.rect(gp=gpar(col="gray", fill=NA))
}

figure11.10 <- function() {
    par(mar=rep(1, 4))
    plot(rnorm(100), rnorm(100), pch=16, col="gray",
         ann=FALSE, axes=FALSE)
    box()

    plotrix::corner.label("top-left", x=-1, y=1)

    gplots::smartlegend(x="right", y="top", 
                        legend="top-right", pch=16, 
                        col="gray", bg="white")

    text(grconvertX(1, "npc"), grconvertY(0, "npc"), 
         adj=c(1, 0), labels="bottom-right")
}

figure11.11 <- function() {
    plot(window(Nile, 1920, 1940))
    TeachingDemos::subplot({ plot(Nile, axes=FALSE, ann=FALSE)
        rect(1920, 0, 1940, 2000, border=NA, col="gray")
        box()
        lines(Nile) }, 
        x=1920, y=1000, size=c(1.5, .75), hadj=0)
}

figure11.12 <- function() {
    kelvin <- pressure$temperature + 273.15

    with(pressure,
         {
             plot(temperature, pressure, axes=FALSE)
             axis(2)
             box()
             staxlab(1, at=temperature, cex=.7)
         })

    with(pressure,
         {
             plot(kelvin, pressure, xlim=c(250, 650))
             axis.break(1)
         })
}

figure11.13 <- function() {
    kelvin <- pressure$temperature + 273.15

    with(pressure,
         revaxis(temperature, pressure))

    plot(kelvin, pressure$pressure)
    TeachingDemos::updateusr(c(0, 1), 0:1, c(-273.15, -272.15), 0:1)
    abline(v=100)
    text(x=100, y=700, " water boils", adj=0)

    pdf("Figures/extra-zoomplot-%d.pdf", onefile=FALSE,
        width=4, height=4)
    dev.control("enable")
    plot(pressure)
    TeachingDemos::zoomplot(c(0, 150), c(0, 3))

    dev.off()
    png("Web/extra-zoomplot%d.png", width=320, height=320)
    dev.control("enable")
    plot(pressure)
    TeachingDemos::zoomplot(c(0, 150), c(0, 3))

    dev.off()
    system("cp Web/extra-zoomplot2.png Web/extra-axisscale3.png")
}
