
figure17.1 <- function() {
    n <- 40
    t <- seq(0, 2*pi, length=n)
    x <- cos(t)
    y <- sin(t)

    for (i in 1:n) {
        plot.new()
        plot.window(c(-1, 1), c(-1, 1))
        lines(x, y)
        points(x[i], y[i], pch=16, cex=2)
        Sys.sleep(.05)
    }
}

figure17.2 <- function() {
    n <- 40
    t <- seq(0, 2*pi, length=n)
    x <- cos(t)
    y <- sin(t)

    orbit <- function() {
        par(pty="s", mar=rep(1, 4))
        for (i in 1:n) {
            plot.new()
            plot.window(c(-1, 1), c(-1, 1))
            lines(x, y)
            points(x[i], y[i], pch=16, cex=2)
        }
    }

    animation::ani.options(interval=0.05, outdir="orbitImages",
                           filename="orbit.html")
    animation::ani.start()
    orbit()
    animation::ani.stop()
}

figure17.3 <- function() {
    plot(mpg ~ disp, mtcars)
    points(mpg ~ disp, mtcars, subset=gear == 3, pch=16)

    plot(mpg ~ disp, mtcars)
    points(mpg ~ disp, mtcars, subset=gear == 4, pch=16)
}

figure17.4 <- function() {
    trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
    print(
        xyplot(mpg ~ disp | factor(gear), mtcars, subset=gear != 5, pch=16)    
    )
}

figure17.5 <- function() {
    mtcars$gear <- factor(mtcars$gear)
    mtcars$cyl <- factor(mtcars$cyl)
    gg <- rggobi::ggobi(mtcars)
}

figure17.6 <- function() {

}

figure17.7 <- function() {
    gg <- rggobi::ggobi(mtcars)
}

figure17.8 <- function() {
    iplots::iplot(mtcars$disp, mtcars$mpg)
    iplots::ibar(mtcars$gear)
}

figure17.9 <- function() {
    iplots::iplot(mtcars$disp, mtcars$mpg)
    iplots::ibar(mtcars$gear)

    iplots::iplot.set(1)

    labels <- mapply("itext", 
                     mtcars$disp, mtcars$mpg, rownames(mtcars), 
                     MoreArgs=list(visible=FALSE), SIMPLIFY=FALSE)
    olds <- NULL
    while (!is.null(iplots::ievent.wait())) {
        if (iplots::iset.sel.changed()) {
            s <- iplots::iset.selected()
            if (length(s) > 1) {
                lapply(labels[s], iplots::iobj.opt, visible = TRUE)
            }
            if (length(olds) > 1) {
                lapply(labels[olds], iplots::iobj.opt, visible = FALSE)
            }
            olds <- s
        }
    }
}

figure17.10 <- function() {

}

figure17.11 <- function() {

}

figure17.12 <- function() {
    # library(latticist)
    # latticist(mtcars, list(xvar="disp", yvar="mpg"), 
    #           use.playwith=FALSE)
    # 
    # 
}

figure17.13 <- function() {
    playwith::playwith(xyplot(mpg ~ disp, mtcars))
    playwith::playwith(xyplot(qsec ~ wt, mtcars), 
                       new=TRUE, link.to=playwith::playDevCur())
}

figure17.15 <- function() {
    drawClock <- function(hour, minute) {
        t <- seq(0, 2*pi, length=13)[-13]
        x <- cos(t)
        y <- sin(t)
    
        grid.newpage()
        pushViewport(dataViewport(x, y, gp=gpar(lwd=4)))
        # Circle with ticks
        grid.circle(x=0, y=0, default.units="native", 
                    r=unit(1, "native"))
        grid.segments(x, y, x*.9, y*.9, default.units="native")
        # Hour hand
        hourAngle <- pi/2 - (hour + minute/60)/12*2*pi
        grid.segments(0, 0, 
                      .6*cos(hourAngle), .6*sin(hourAngle), 
                      default.units="native", gp=gpar(lex=4))
        # Minute hand
        minuteAngle <- pi/2 - (minute)/60*2*pi
        grid.segments(0, 0, 
                      .8*cos(minuteAngle), .8*sin(minuteAngle), 
                      default.units="native", gp=gpar(lex=2))
        grid.circle(0, 0, default.units="native", r=unit(1, "mm"),
                    gp=gpar(fill="white"))
    }

    window <- gWidgets::gwindow("Clock")

    allContent <- gWidgets::ggroup(container=window, horizontal=FALSE)

    graphicTime <- gWidgets::ggraphics(container=allContent)

    timeContent <- gWidgets::ggroup(container=allContent)

    textLabel <- gWidgets::glabel("")

    # randomizeTime <- function(h, ...) {
    #     hour <- sample(1:12, 1)
    #     minute <- sample(seq(0, 55, 5), 1)
    #     drawClock(hour, minute)
    #     gWidgetsRGtk2::visible(textLabel) <- FALSE
    #     gWidgetsRGtk2::svalue(textLabel) <- paste(hour, 
    #                                               sprintf("%02d", minute), 
    #                                               sep=":")
    # }

    # reset <- gWidgets::gbutton("Randomize Time", 
    #                            handler=randomizeTime)

    # textButton <- gWidgets::gbutton("Show Time", 
    #                                 handler=function(h, ...) {
    #                                     gWidgetsRGtk2::visible(textLabel) <- TRUE
    #                                 })

    # gWidgetsRGtk2::add(timeContent, reset)
    # gWidgetsRGtk2::add(timeContent, textButton)
    # gWidgetsRGtk2::add(timeContent, textLabel)
}

# figure17.16 <- function() {

#     doc <- SVGAnnotation::svgPlot({ par(mfrow=c(2, 1), cex=.7, 
#                                         mar=c(5.1, 4.1, 1, 1))
#         plot(mpg ~ disp, mtcars, cex=2)
#         plot(qsec ~ wt, mtcars, cex=2) },
#         width=4, height=8)
#     SVGAnnotation::linkPlots(doc)
#     XML::saveXML(doc, "linkedplots.svg")
# }
