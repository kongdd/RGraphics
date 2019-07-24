figure6.1 <- function() {
    pushViewport(viewport(layout=grid.layout(2, 2), gp=gpar(cex=0.6, fill=NA)))
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
    pushViewport(plotViewport(c(5, 4, 2, 2)))
    pushViewport(dataViewport(pressure$temperature, 
                              pressure$pressure,
                              name="plotRegion"))

    grid.points(pressure$temperature, pressure$pressure, 
                gp=gpar(cex=0.5))
    grid.rect()
    grid.xaxis()
    grid.yaxis()
    grid.text("temperature", y=unit(-3, "line"))
    grid.text("pressure", x=unit(-3, "line"), rot=90)

    popViewport(3)
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
    pushViewport(plotViewport(c(5, 4, 2, 2)))
    pushViewport(dataViewport(pressure$temperature, 
                              pressure$pressure,
                              name="plotRegion"))

    grid.points(pressure$temperature, pressure$pressure, pch=2, 
                gp=gpar(cex=0.5))
    grid.rect()
    grid.xaxis()
    grid.yaxis()
    grid.text("temperature", y=unit(-3, "line"))
    grid.text("pressure", x=unit(-3, "line"), rot=90)

    popViewport(3)
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
    pushViewport(plotViewport(c(5, 4, 2, 2)))
    pushViewport(dataViewport(pressure$temperature, 
                              pressure$pressure,
                              name="plotRegion"))

    grid.points(pressure$temperature, pressure$pressure, pch=2, 
                gp=gpar(cex=0.5))
    grid.rect()
    grid.xaxis()
    grid.yaxis()
    grid.text("temperature", y=unit(-3, "line"))
    grid.text("pressure", x=unit(-3, "line"), rot=90)

    upViewport(2)
    grid.rect(gp=gpar(lty="dashed"))

    downViewport("plotRegion")
    grid.text("Pressure (mm Hg)\nversus\nTemperature (Celsius)",
              x=unit(150, "native"), y=unit(600, "native"))
}

figure6.2 <- function() {
    grid.rect(gp=gpar(col="gray"))
    grid.circle(x=seq(0.1, 0.9, length=100), 
                y=0.5 + 0.4*sin(seq(0, 2*pi, length=100)),
                r=abs(0.1*cos(seq(0, 2*pi, length=100))))
}

figure6.3 <- function() {
    grid.rect(gp=gpar(col="gray"))
    grid.circle(c(.1, .3, .4, .6, .7, .9), 
                c(.25, .75), r=unit(1, "mm"),
                gp=gpar(col=NA, fill="gray"))
    grid.curve(x1=.1, y1=.25, x2=.3, y2=.75)
    grid.curve(x1=.4, y1=.25, x2=.6, y2=.75,
               square=FALSE, ncp=8, curvature=.5)
    grid.curve(x1=.7, y1=.25, x2=.9, y2=.75,
               square=FALSE, angle=45, shape=-1)
}

figure6.4 <- function() {
    grid.rect(gp=gpar(col="gray"))
    angle <- seq(0, 2*pi, length=50)
    grid.lines(x=seq(0.1, 0.5, length=50), 
               y=0.5 + 0.3*sin(angle), arrow=arrow())
    grid.segments(6:8/10, 0.2, 7:9/10, 0.8,
                  arrow=arrow(angle=15, type="closed"))
}

figure6.5 <- function() {
    grid.rect(gp=gpar(col="gray"))
    angle <- seq(0, 2*pi, length=10)[-10]
    grid.polygon(x=0.25 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
                 gp=gpar(fill="gray"))
    grid.polygon(x=0.75 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
                 id=rep(1:3, each=3),
                 gp=gpar(fill="gray"))
}

figure6.6 <- function() {
    grid.rect(gp=gpar(col="gray"))
    angle <- seq(0, 2*pi, length=10)[-10]
    grid.path(x=0.25 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
              gp=gpar(fill="gray"))
    grid.path(x=c(0.75 + 0.15*cos(angle), .7, .7, .8, .8),
              y=c(0.5 + 0.3*sin(angle),  .4, .6, .6, .4), 
              id=rep(1:2, c(9, 4)),
              gp=gpar(fill="gray"))
}

figure6.7 <- function() {
    grid.rect(gp=gpar(col="gray"))
    pushViewport(viewport(gp=gpar(col="gray")))
    grid.text("very snug", 0.4, unit(1, "in"), just=c("left", "bottom"))
    grid.lines(x=0.4, y=unit(0:1, "in"), arrow=arrow(ends="both",
                                                     length=unit(1, "mm")))
    grid.text("1 inch", unit(0.4, "npc") + unit(0.5, "line"), 
              unit(0.5, "in"), rot=90)
    grid.lines(x=c(0, 0.4), y=unit(1, "in"), arrow=arrow(ends="both",
                                                         length=unit(1, "mm")))
    grid.text(unit(0.4, "npc"), 0.2, unit(1, "in") + unit(0.5, "line"))
    popViewport()
    pushViewport(viewport(gp=gpar(fill=NA)))
    grid.rect(x=unit(0.4, "npc"), y=unit(1, "in"),
              width=stringWidth("very snug"), 
              height=unit(1, "line"),
              just=c("left", "bottom"))
}

figure6.8 <- function() {
    grid.rect(gp=gpar(col="gray"))
    pushViewport(viewport(gp=gpar(fontsize=10)))
    grid.rect(x=0.33, height=0.7, width=0.2, gp=gpar(fill="black"))
    grid.rect(x=0.66, height=0.7, width=0.2)
    grid.text("grid.rect()", x=0.66, rot=90)
    grid.text("grid.rect(gp=gpar(fill=\"black\"))", x=0.33, rot=90, 
              gp=gpar(fontsize=8, col="white"))
    popViewport()
}

figure6.9 <- function() {
    grid.rect(gp=gpar(col="gray"))
    levels <- round(seq(90, 10, length=25))
    grays <- paste("gray", c(levels, rev(levels)), sep="")
    grid.circle(x=seq(0.1, 0.9, length=100), 
                y=0.5 + 0.4*sin(seq(0, 2*pi, length=100)),
                r=abs(0.1*cos(seq(0, 2*pi, length=100))),
                gp=gpar(col=grays))
}

figure6.10 <- function() {
    grid.rect(gp=gpar(col="gray"))
    angle <- seq(0, 2*pi, length=11)[-11]
    grid.polygon(x=0.25 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
                 id=rep(1:2, c(7, 3)),
                 gp=gpar(fill=c("gray", "white")))
    angle[4] <- NA
    grid.polygon(x=0.75 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
                 id=rep(1:2, c(7, 3)),
                 gp=gpar(fill=c("gray", "white")))

    angle <- seq(0, 2*pi, length=11)[4]
    grid.text("NA", x=0.75 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle),
              gp=gpar(col="gray"))
}

figure6.11 <- function() {
    vp1 <- 
        viewport(x=unit(0.4, "npc"), y=unit(1, "cm"),
                 width=stringWidth("very very snug indeed"), 
                 height=unit(6, "line"),
                 just=c("left", "bottom"))

    grid.show.viewport(scale.col="gray", border.fill="white", vp.col="black", vp.fill="gray", vp1)
    grid.rect(gp=gpar(col="white", fill=NA, lwd=3))
    pushViewport(viewport(.5, .5, .8, .8))
    pushViewport(vp1)
    grid.rect(gp=gpar(fill=NA))
    grid.text("very very snug indeed", 
              gp=gpar(col="white"))
    popViewport(2)
}

figure6.12 <- function() {
    grid.rect(gp=gpar(col="gray"))
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))
    pushViewport(viewport(width=0.8, height=0.5, angle=10, 
                          name="vp1"))
    grid.rect()
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))
}

figure6.13 <- function() {
    grid.rect(gp=gpar(col="gray"))
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))
    pushViewport(viewport(width=0.8, height=0.5, angle=10, 
                          name="vp1"))
    grid.rect()
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))

    pushViewport(viewport(width=0.8, height=0.5, angle=10, 
                          name="vp2"))
    grid.rect()
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))
}

figure6.14 <- function() {
    grid.rect(gp=gpar(col="gray"))
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))
    pushViewport(viewport(width=0.8, height=0.5, angle=10, 
                          name="vp1"))
    grid.rect()
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))

    pushViewport(viewport(width=0.8, height=0.5, angle=10, 
                          name="vp2"))
    grid.rect()
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))

    popViewport()
    grid.text("bottom-right corner", 
              x=unit(1, "npc") - unit(1, "mm"),
              y=unit(1, "mm"), just=c("right", "bottom"))
}

figure6.15 <- function() {
    pushViewport(viewport(gp=gpar(fill=NA)))
    grid.rect(gp=gpar(col="gray"))
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))
    pushViewport(viewport(width=0.8, height=0.5, angle=10, 
                          name="vp1"))
    grid.rect()
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))

    pushViewport(viewport(width=0.8, height=0.5, angle=10, 
                          name="vp2"))
    grid.rect()
    grid.text("top-left corner", x=unit(1, "mm"),
              y=unit(1, "npc") - unit(1, "mm"), 
              just=c("left", "top"))

    popViewport()
    grid.text("bottom-right corner", 
              x=unit(1, "npc") - unit(1, "mm"),
              y=unit(1, "mm"), just=c("right", "bottom"))

    upViewport()
    grid.text("bottom-right corner", 
              x=unit(1, "npc") - unit(1, "mm"),
              y=unit(1, "mm"), just=c("right", "bottom"))
    downViewport("vp1")
    grid.rect(width=unit(1, "npc") + unit(2, "mm"),
              height=unit(1, "npc") + unit(2, "mm"))
}

figure6.16 <- function() {
    grid.rect(gp=gpar(col="gray"))
    pushViewport(viewport(gp=gpar(fill=NA)))
    pushViewport(viewport(width=.5, height=.5, clip="on"))
    grid.rect()
    grid.circle(r=.7, gp=gpar(lwd=20))

    pushViewport(viewport(clip="inherit"))
    grid.circle(r=.7, gp=gpar(lwd=10, col="gray"))

    pushViewport(viewport(clip="off"))
    grid.circle(r=.7)
    popViewport(3)
}

figure6.17 <- function() {
    circText <- function(lab, x, y, suffix) {
        grid.circle(x, y, r=unit(3, "mm"), 
                    name=paste(lab, suffix, sep="-"))
        grid.text(lab, x, y,
                  gp=if (lab == "ROOT") gpar(cex=.7) else NULL)
    }
    edge <- function(a, b, angle) {
        grid.segments(grobX(a, angle), grobY(a, angle),
                      grobX(b, 180 + angle), grobY(b, 180 + angle),
                      arrow=arrow(length=unit(2, "mm"),
                                  type="closed"),
                      gp=gpar(fill="black"))
    }
    grid.newpage()
    pushViewport(viewport(width=.9, height=.9,
                          layout=grid.layout(2, 2),
                          gp=gpar(cex=.5)))
    pushViewport(viewport(layout.pos.col=1, 
                          layout.pos.row=1))
    grid.rect(width=.9, height=.9, gp=gpar(col="gray"))
    circText("ROOT", .5, .8, 1)
    circText("A", .3, .6, 1)
    circText("B", .5, .6, 1)
    circText("C", .7, .6, 1)
    edge("ROOT-1", "A-1", 225)
    edge("ROOT-1", "B-1", 270)
    edge("ROOT-1", "C-1", 315)
    popViewport()
    pushViewport(viewport(layout.pos.col=2, 
                          layout.pos.row=1))
    grid.rect(width=.9, height=.9, gp=gpar(col="gray"))
    circText("ROOT", .5, .8, 2)
    circText("A", .5, .6, 2)
    circText("B", .5, .4, 2)
    circText("C", .5, .2, 2)
    edge("ROOT-2", "A-2", 270)
    edge("A-2", "B-2", 270)
    edge("B-2", "C-2", 270)
    popViewport()
    pushViewport(viewport(layout.pos.col=1, 
                          layout.pos.row=2))
    grid.rect(width=.9, height=.9, gp=gpar(col="gray"))
    circText("ROOT", .5, .8, 3)
    circText("A", .5, .6, 3)
    circText("B", .4, .4, 3)
    circText("C", .6, .4, 3)
    edge("ROOT-3", "A-3", 270)
    edge("A-3", "B-3", 244)
    edge("A-3", "C-3", 296)
    popViewport()
}

figure6.18 <- function() {
    grid.rect(gp=gpar(col="gray"))
    pushViewport(viewport(gp=gpar(fill="gray", fontsize=10)))
    grid.text("viewport(gp=gpar(fill=\"gray\"))", y=0.925)
    grid.rect(x=0.33, height=0.7, width=0.2)
    grid.text("grid.rect()", x=0.33, rot=90)
    grid.rect(x=0.66, height=0.7, width=0.2, gp=gpar(fill="black"))
    grid.text("grid.rect(gp=gpar(fill=\"black\"))", x=0.66, rot=90, 
              gp=gpar(fontsize=8, col="white"))
    popViewport()
}

figure6.19 <- function() {
    labelvp <- function(name, col="gray", tcol="white", clipOff=TRUE) {
        seekViewport(name)
        if (clipOff)
            pushViewport(viewport(clip="off"))
        grid.rect(gp=gpar(col=col, lwd=5, fill=NA))
        grid.rect(x=0, y=1, width=unit(1, "strwidth", name) + unit(2, "mm"),
                  height=unit(1, "line"), just=c("left", "top"),
                  gp=gpar(fill=col, col=NA))
        grid.text(name, x=unit(1, "mm"), y=unit(1, "npc") - unit(1, "mm"),
                  just=c("left", "top"), gp=gpar(col=tcol))
        upViewport(0)
    }

    vplay <- grid.layout(3, 3, 
                         respect=rbind(c(0, 0, 0), 
                                       c(0, 1, 0), 
                                       c(0, 0, 0)))

    pushViewport(viewport(width=0.95, height=0.95))
    grid.rect(gp=gpar(col="light gray"))
    pushViewport(viewport(layout=vplay))

    pushViewport(viewport(layout.pos.col=2, name="col2"))
    upViewport()
    pushViewport(viewport(layout.pos.row=2, name="row2"))

    labelvp("col2", "black")
    labelvp("row2")
}

figure6.20 <- function() {
    unitlay <- 
        grid.layout(3, 3, 
                    widths=unit(c(1, 1, 2), 
                                c("in", "null", "null")), 
                    heights=unit(c(3, 1, 1), 
                                 c("line", "null", "null")))

    pushViewport(viewport(gp=gpar(cex=0.8)))
    grid.show.layout(unitlay, bg="white", 
                     cell.border="black", cell.fill="gray90", 
                     label.col="black", unit.col="black",
                     newpage=FALSE)
    grid.rect(gp=gpar(col="white", lwd=3, fill=NA))
    popViewport()
}

figure6.21 <- function() {
    gridfun <- function() {
        pushViewport(viewport(layout=grid.layout(1, 2)))
        pushViewport(viewport(layout.pos.col=1))
        grid.rect()
        grid.text("black")
        grid.text("&", x=1)
        popViewport()
        pushViewport(viewport(layout.pos.col=2, clip="on"))
        grid.rect(gp=gpar(fill="black"))
        grid.text("white", gp=gpar(col="white"))
        grid.text("&", x=0, gp=gpar(col="white"))
        popViewport(2)
    }

    grid.rect(gp=gpar(col="gray"))
    w <- unit(1, "npc") - unit(15, "mm")
    x <- unit.c(unit(5, "mm"),
                unit(5, "mm") + 1/3*w,
                unit(5, "mm") + 1/3*w + unit(5, "mm"),
                unit(1, "npc") - unit(5, "mm"))
    y <- unit.c(unit(5, "mm"),
                unit(5, "mm") + 2/3*w,
                unit(5, "mm") + 2/3*w + unit(5, "mm"),
                unit(1, "npc") - unit(5, "mm"))
    grid.segments(x, 0, x, 1,
                  gp=gpar(col="gray", lty="dashed"))
    grid.segments(0, y, 1, y,
                  gp=gpar(col="gray", lty="dashed"))
    pushViewport(
        viewport(
            layout=grid.layout(5, 5, 
                               widths=unit(c(5, 1, 5, 2, 5),
                                           c("mm", "null", "mm",
                                             "null", "mm")),  
                               heights=unit(c(5, 1, 5, 2, 5),
                                            c("mm", "null", "mm",
                                              "null", "mm")))))
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
    gridfun()
    popViewport()

    pushViewport(viewport(layout.pos.col=4, layout.pos.row=4))
    gridfun()
    popViewport(2)
}

figure6.22 <- function() {
    n <- 7
    primtest2 <- function(nas, na) {
        angle <- seq(0, 2*pi, length=n+1)[-(n+1)]
        y <- 0.5 + 0.4*sin(angle)
        x <- 0.5 + 0.4*cos(angle)
        if (any(nas))
            grid.text(paste("NA", (1:n)[nas], sep=""),
                      x[nas], y[nas], gp=gpar(col="gray"))
        x[nas] <- na
        y[nas] <- na
        grid.polygon(x, y, gp=gpar(fill="light gray", col=NA))
        grid.lines(x, y, arrow=arrow(), gp=gpar(lwd=5))
        grid.move.to(x[1], y[1])
        for (i in 2:n) {
            grid.line.to(x[i], y[i], gp=gpar(col="white"))
        }
    }
    celltest <- function(r, c, nas, na) {
        pushViewport(viewport(layout.pos.col=c,
                              layout.pos.row=r))
        primtest2(nas, na)
        grid.rect(width=0.9, height=0.9, gp=gpar(col="gray", fill=NA))
        popViewport()
    }
    cellnas <- function(i) {
        temp <- rep(FALSE, n)
        temp[i] <- TRUE
        temp[n-3+i] <- TRUE
        temp
    }
    pushViewport(viewport(width=.8, height=.8, 
                          layout=grid.layout(2, 2),
                          gp=gpar(cex=0.7)))
    celltest(1, 1, rep(FALSE, n), NA)
    celltest(1, 2, cellnas(1), NA)
    celltest(2, 1, cellnas(2), NA)
    celltest(2, 2, cellnas(3), NA)
    popViewport()
}

figure6.23 <- function() {
    trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
    trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
    print(
        xyplot(mpg ~ disp | factor(gear), data=mtcars,
               panel=function(subscripts, ...) {
                   grid.text(paste("n =", length(subscripts)),
                             unit(1, "npc") - unit(1, "mm"),
                             unit(1, "npc") - unit(1, "mm"),
                             just=c("right", "top"))
                   panel.xyplot(subscripts=subscripts, ...)
               })
    
    )
}

figure6.24 <- function() {
    trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
    grid.newpage()
    pushViewport(viewport(x=0, width=.4, just="left"))
    print(barchart(table(mtcars$gear)),
          newpage=FALSE)
    popViewport()
    pushViewport(viewport(x=.4, width=.6, just="left"))
    print(xyplot(mpg ~ disp, data=mtcars,
                 group=gear, 
                 auto.key=list(space="right"),
                 par.settings=list(superpose.symbol=list(pch=c(1, 3, 16),
                                                         fill="white"))),
          newpage=FALSE)
    popViewport()
}

figure6.25 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$gear <- as.factor(mtcars$gear)
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    print(
        ggplot(mtcars2, aes(x=disp, y=mpg)) +
            geom_point()
    
    )
    downViewport("panel.3-4-3-4")
    grid.text(paste("n =", nrow(mtcars2)),
              x=unit(1, "npc") - unit(1, "mm"), 
              y=unit(1, "npc") - unit(1, "mm"),
              just=c("right", "top"))
}

figure6.26 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$gear <- as.factor(mtcars$gear)
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    grid.newpage()
    pushViewport(viewport(x=0, width=1/3, just="left"))
    print(ggplot(mtcars2, aes(x=trans)) + 
              geom_bar(),
          newpage=FALSE)
    popViewport()
    pushViewport(viewport(x=1/3, width=2/3, just="left"))
    print(ggplot(mtcars2, aes(x=disp, y=mpg)) +
              geom_point(aes(color=trans)) +
              scale_color_manual(values=gray(2:1/3)),
          newpage=FALSE)
    popViewport()
}
