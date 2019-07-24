#' @name figure04
#' @title figure04
figure04 <- function(){
    
}
NULL


figure4.1 <- function() {
    trellis.par.set(list(dot.symbol=list(pch=1)))
    print(
        xyplot(pressure ~ temperature, pressure)
    )
}

figure4.2 <- function() {
    tplot <- xyplot(pressure ~ temperature, pressure)

    trellis.par.set(list(dot.symbol=list(pch=1)))
    print(
        xyplot(pressure ~ temperature, pressure,
               type="o", pch=16, lty="dashed", 
               main="Vapor Pressure of Mercury")
    )
}

figure4.3 <- function() {
    x <- 1:5
    y <- 1:5
    g <- factor(1:5)
    types <- c("barchart", "bwplot", "densityplot", "dotplot",
               "histogram", "qqmath", "stripplot", "qq",
               "xyplot", "levelplot", "contourplot",
               "cloud", "wireframe", "splom", "parallel")
    angle <- seq(0, 2*pi, length=19)[-19]
    xx <- cos(angle)
    yy <- sin(angle)
    gg <- factor(rep(1:3, each=6))

    aaa <- seq(0, pi, length=10)
    xxx <- rep(aaa, 10)
    yyy <- rep(aaa, each=10)
    zzz <- sin(xxx) + sin(yyy)

    doplot <- function(name, ...) {
        do.call(name, 
                list(..., scales=list(draw=FALSE), xlab=NULL, ylab=NULL,
                     strip=function(which.panel, ...) { 
                         grid.rect(gp=gpar(fill="gray90")); grid.text(name) 
                     }))
    }
    plot <- vector("list", 15)
    plot[[1]] <- doplot("barchart", y ~ g | 1)
    plot[[2]] <- doplot("bwplot", yy ~ gg | 1, 
                        par.settings=list(box.umbrella=list(lwd=0.5)))
    plot[[3]] <- doplot("densityplot", ~ yy | 1)
    plot[[4]] <- doplot("dotplot", g ~ y | 1)
    plot[[5]] <- doplot("histogram", ~ xx | 1)
    plot[[6]] <- doplot("qqmath", ~ yy | 1)
    plot[[7]] <- doplot("stripplot", yy ~ gg | 1)
    plot[[8]] <- doplot("qq", gg ~ yy | rep(1, 18), subset=gg != 3)
    plot[[9]] <- doplot("xyplot", xx ~ yy | 1)
    plot[[10]] <- doplot("levelplot", zzz ~ xxx + yyy | 1, colorkey=FALSE)
    plot[[11]] <- doplot("contourplot", zzz ~ xxx + yyy | 1, labels=FALSE, cuts=8)
    plot[[12]] <- doplot("cloud", zzz ~ xxx + yyy | 1, zlab=NULL, zoom=0.9, 
                         par.settings=list(box.3d=list(lwd=0.1)))
    plot[[13]] <- doplot("wireframe", zzz ~ xxx + yyy | 1, zlab=NULL, zoom=0.9,
                         drape=TRUE, par.settings=list(box.3d=list(lwd=0.1)),
                         colorkey=FALSE)
    plot[[14]] <- doplot("splom", ~ data.frame(x=xx[1:10], y=yy[1:10]) | 1, 
                         pscales=0)
    plot[[15]] <- doplot("parallel", ~ as.data.frame(split(yy, gg)) | 1)

    grid.newpage()
    pushViewport(viewport(layout=grid.layout(4, 4)))
    for (i in 1:15) {
        pushViewport(viewport(layout.pos.col=((i - 1) %% 4) + 1,
                              layout.pos.row=((i - 1) %/% 4) + 1))
        print(plot[[i]], newpage=FALSE, 
              panel.width=list(1.025, "inches"),
              panel.height=list(1.025, "inches"))
        popViewport()
    }
    popViewport()
}

figure4.4 <- function() {
    print(xyplot(mpg ~ disp, data=mtcars))
}

figure4.5 <- function() {
    trellis.par.set(list(dot.symbol=list(pch=1)))
    trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
    print(
        xyplot(mpg ~ disp | factor(gear), data=mtcars)
    )
}

figure4.6 <- function() {
    trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
    print(
        xyplot(mpg ~ disp, data=mtcars,
               group=gear, 
               auto.key=list(space="right"),
               par.settings=list(superpose.symbol=list(pch=c(1, 3, 16),
                                                       fill="white")))
    )
}

figure4.7 <- function() {
    trellis.par.set(list(dot.symbol=list(pch=1)))
    print(
        xyplot(mpg ~ disp | factor(gear), data=mtcars,
               layout=c(1, 3), aspect=1)
    )
}

figure4.8 <- function() {
    trellis.par.set(list(fontsize=list(text=10)))
    trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
    plot1 <- xyplot(mpg ~ disp, data=mtcars, 
                    aspect=1, xlim=c(65, 480), ylim=c(9, 35),
                    subset=gear == 5)
    plot2 <- xyplot(mpg ~ disp, data=mtcars, 
                    aspect=1, xlim=c(65, 480), ylim=c(9, 35),
                    subset=gear == 4)
    plot3 <- xyplot(mpg ~ disp, data=mtcars, 
                    aspect=1, xlim=c(65, 480), ylim=c(9, 35),
                    subset=gear == 3)
    print(plot1, position=c(0, 2/3, 1, 1), more=TRUE)
    print(plot2, position=c(0, 1/3, 1, 2/3), more=TRUE)
    print(plot3, position=c(0, 0, 1, 1/3))
}

figure4.9 <- function() {
    trellis.par.set(list(fontsize=list(text=10)))
    print(
        xyplot(mpg ~ disp | factor(gear), data=mtcars,
               layout=c(3, 1), aspect=1,
               scales=list(y=list(at=seq(10, 30, 10))),
               ylab="miles per gallon",
               xlab=expression(paste("displacement (", inch^3, ")")))
    )
}

figure4.10 <- function() {
    trellis.par.set(list(fontsize=list(text=10)))
    print(
        xyplot(mpg ~ disp | factor(gear), data=mtcars,
               layout=c(3, 1), aspect=1,
               panel=function(...) {
                   panel.xyplot(...)
                   panel.abline(h=29, lty="dashed")
                   panel.text(470, 29.5, "efficiency criterion",
                              adj=c(1, 0), cex=.7)
               })
    )
}

figure4.11 <- function() {
    trellis.par.set(list(fontsize=list(text=10)))
    gray.colors <- function(n) { 
        adjustcolor(gray(n:1/n), alpha.f=.7) 
    }
    print(
        xyplot(mpg ~ disp | factor(gear), data=mtcars,
               layout=c(3, 1), aspect=1,
               panel=function(x, y, ...) {
                   panel.lmline(x, y)
                   panel.xyplot(x, y, ...)
               })
    )
}

figure4.12 <- function() {
    trellis.par.set(list(fontsize=list(text=9, points=8)))
    show.settings()
}

figure4.13 <- function() {
    doplot <- function(name, ...) {
        do.call(name, 
                list(..., scales=list(draw=FALSE), xlab=NULL, ylab="",
                     strip=function(which.panel, ...) { 
                         grid.rect(gp=gpar(fill="gray90")); grid.text(name) 
                     }))
    }

    plot <- vector("list", 4)
    plot[[1]] <- doplot("ecdfplot", ~ rnorm(10) | 1)
    plot[[2]] <- doplot("rootogram", ~ rpois(50, 10) | 1, 
                        dfun=function(x) dpois(x, 10))
    plot[[3]] <- doplot("segplot", factor(1:5) ~ rnorm(5) + rnorm(5) | 1)
    plot[[4]] <- doplot("tileplot", 
                        1:10 ~ rnorm(10) + rnorm(10) | 1,
                        colorkey=FALSE, aspect="fill")

    grid.newpage()
    pushViewport(viewport(layout=grid.layout(1, 4)))
    for (i in 1:4) {
        pushViewport(viewport(layout.pos.col=i,
                              layout.pos.row=1))
        print(plot[[i]], newpage=FALSE, 
              panel.width=list(1.025, "inches"),
              panel.height=list(1.025, "inches"))
        popViewport()
    }
    popViewport()
}
