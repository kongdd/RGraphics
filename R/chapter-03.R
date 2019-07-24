figure3.1 <- function() {
    par(oma=rep(3, 4), bg="gray80")
    plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
    box("outer", col="gray")
    # set clipping to figure region
    par(xpd=TRUE)
    # deliberately draw a stupidly large rectangle
    rect(-1, -1, 2, 2, col="gray90")
    box("figure")
    # set clipping back to plot region
    par(xpd=FALSE)
    # deliberately draw a stupidly large rectangle
    rect(-1, -1, 2, 2, col="gray80")
    box("plot", lty="dashed")
    text(.5, .5, "Plot Region")
    mtext("Figure Region", side=3, line=2)
    for (i in 1:4)
        mtext(paste("Outer margin", i), side=i, line=1, outer=TRUE)

}

figure3.2 <- function() {
    par(oma=rep(3, 4), mfrow=c(3,2), bg="gray80")
    for (i in 1:6) {
        if (i == 3) {
            omar <- par(mar=c(2, 2, 2, 1))  
            plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
            par(xpd=TRUE)
            rect(-1, -1, 2, 2, col="gray90")
            box("figure")
            par(xpd=FALSE)
            rect(-1, -1, 2, 2, col="gray80")
            box("plot", lty="dashed")
            text(.5, .5, "Current Plot Region", cex=1.5)
            mtext("Current Figure Region", side=3)
            par(omar)
        } else {
            omar <- par(mar=rep(0, 4))  
            plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
            par(xpd=TRUE)
            rect(-1, -1, 2, 2, col="gray90")
            box("figure")
            text(.5, .5, paste("Figure", i), cex=1.5)
            par(omar)
        }
    }
    box("outer", col="gray")
    for (i in 1:4)
        mtext(paste("Outer margin", i), side=i, line=1, outer=TRUE)

}

figure3.3 <- function() {
    par(mar=c(3, 6, 2, 2), xaxs="i", yaxs="i", xpd=FALSE, las=1)
    plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
    box("figure")
    rect(0, 0, 1, 1, col="light gray", border="gray")
    axis(1, at=c(0, 1), c("", ""))
    mtext("Min x-value", side=1, adj=0, line=1)
    mtext("Max x-value", side=1, adj=1, line=1)
    axis(2, at=c(0, 1), c("", ""))
    mtext("Min y-value", side=2, at=0, adj=1, line=1)
    mtext("Max y-value", side=2, at=1, adj=1, line=1)
    lines(c(.6, .6, 0), c(0, .6, .6), lty="dashed")
    text(.6, .6, expression(paste("The location ", 
                                  group("(",list(x[i], y[i]),")"))), pos=3)
    points(.6, .6, pch=16)
    axis(1, at=.6, "")
    mtext(expression(x[i]), side=1, at=.6, line=.7)
    axis(2, at=.6, "")
    mtext(expression(y[i]), side=2, at=.6, line=.7)
}

figure3.4 <- function() {
    pushViewport(viewport(layout=grid.layout(3, 1, 
                                             heights=unit(rep(1, 3), c("null", "cm", "null")))))
    pushViewport(viewport(layout.pos.row=1))
    grid.rect()
    pushViewport(plotViewport(c(5, 5, 3, 2), xscale=c(0, 11)))
    grid.rect(gp=gpar(col="gray"))
    grid.text("Current Plot", gp=gpar(col="gray"))
    grid.rect(0, unit(-5, "lines"), 1, unit(5, "lines"),
              just=c("left", "bottom"), gp=gpar(col="gray", fill="light gray"))
    grid.text("Figure\nMargin\n1", y=unit(-2.5, "lines"))
    grid.lines(c(0, 1), c(0, 0))
    grid.segments(c(0, 1), c(0, 0), c(0, 1), unit(c(.5, .5), "lines"))
    grid.text(c("xmin", "xmax"), c(0, 1), unit(c(1, 1), "lines"))
    grid.lines(c(0, 0), unit(c(-1, -4), "lines"))
    grid.segments(c(0, 0), unit(c(-1, -4), "lines"), 
                  unit(c(-.5, -.5), "lines"), unit(c(-1, -4), "lines"))
    grid.text(c("0 lines", "3 lines"),
              unit(c(-1, -1), "lines"), unit(c(-1, -4), "lines"),
              just=c("right", "bottom"))
    popViewport(2)
    pushViewport(viewport(layout.pos.row=3))
    grid.rect()
    pushViewport(plotViewport(c(5, 5, 3, 2), yscale=c(0, 11)))
    grid.rect(gp=gpar(col="gray"))
    grid.text("Current Plot", gp=gpar(col="gray"))
    grid.rect(unit(-5, "lines"), 0, unit(5, "lines"), 1,
              just=c("left", "bottom"), gp=gpar(col="gray", fill="light gray"))
    grid.text("Figure\nMargin\n2", x=unit(-2.5, "lines"))
    grid.lines(c(0, 0), c(0, 1))
    grid.segments(c(0, 0), c(0, 1), unit(c(.5, .5), "lines"), c(0, 1))
    grid.text(c("ymin", "ymax"), unit(c(1, 1), "lines"), c(0, 1), just="left")
    grid.lines(unit(c(0, -3), "lines"), c(0, 0))
    grid.segments(unit(c(0, -3), "lines"), c(0, 0), 
                  unit(c(0, -3), "lines"), unit(c(-.5, -.5), "lines"))
    grid.text(c("0 lines", "3 lines"),
              unit(c(0, -3), "lines"), unit(c(-1, -1), "lines"),
              rot=90, just=c("right", "bottom"))
    popViewport(2)
    popViewport()
}

figure3.5 <- function() {
    pushViewport(viewport(layout=grid.layout(3, 1, 
                                             heights=unit(c(1, 1, 1), c("null", "cm", "null")))))

    # First page
    pushViewport(viewport(layout.pos.row=3, 
                          layout=grid.layout(3, 4, 
                                             widths=unit(c(2.5, 1, 1, 1), c("cm", "null", "null", "cm")),
                                             heights=unit(c(1, 1, 2.5), c("cm", "null", "cm")))))
    grid.rect(gp=gpar(col="black"))
    for (i in 2) {
        for (j in 2:3) {
            pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
            grid.rect(gp=gpar(col="gray"))
            pushViewport(plotViewport(c(2, 2, 1, 1), xscale=c(0, 11),
                                      gp=gpar(col="gray")))
            grid.rect(gp=gpar(col="gray"))
            grid.text(paste("Plot", j - 1))
            popViewport()  
            popViewport()
        }
    }
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
    grid.rect(gp=gpar(col="gray", fill="light gray"))
    grid.text("Outer\nMargin\n2")
    grid.lines(c(1, 1), c(0, 1))
    grid.segments(c(1, 1), c(0, 1), 
                  unit(1, "npc") + unit(c(.5, .5), "lines"),
                  c(0, 1))
    grid.text(0:1, 
              unit(1, "npc") + unit(c(1, 1), "lines"),
              c(0, 1))
    grid.lines(unit(1, "npc") - unit(c(0, 3), "lines"), c(0, 0))
    grid.segments(unit(1, "npc") - unit(c(0, 3), "lines"), 
                  c(0, 0), 
                  unit(1, "npc") - unit(c(0, 3), "lines"), 
                  unit(c(-.5, -.5), "lines"))
    grid.text(c("0 lines", "3 lines"),
              unit(1, "npc") - unit(c(0, 3), "lines"),
              unit(c(-1, -1), "lines"),
              rot=90, just=c("right", "bottom"))
    popViewport(2)

    # Second page
    pushViewport(viewport(layout.pos.row=1, 
                          layout=grid.layout(3, 4, 
                                             widths=unit(c(2.5, 1, 1, 1), c("cm", "null", "null", "cm")),
                                             heights=unit(c(1, 1, 2.5), c("cm", "null", "cm")))))
    grid.rect(gp=gpar(col="black"))
    for (i in 2) {
        for (j in 2:3) {
            pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
            grid.rect(gp=gpar(col="gray"))
            pushViewport(plotViewport(c(2, 2, 1, 1), xscale=c(0, 11),
                                      gp=gpar(col="gray")))
            grid.rect(gp=gpar(col="gray"))
            grid.text(paste("Plot", j - 1))
            popViewport()  
            popViewport()
        }
    }
    pushViewport(viewport(layout.pos.row=3, layout.pos.col=2:3))
    grid.rect(gp=gpar(col="gray", fill="light gray"))
    grid.text("Outer Margin 1")
    grid.lines(c(0, 1), c(1, 1))
    grid.segments(c(0, 1), 
                  unit(c(1, 1), "npc"),
                  c(0, 1),
                  unit(c(1, 1), "npc") + unit(.5, "lines"))
    # grid.rect(c(0, 1), 
    #     unit(c(1, 1), "npc") + unit(1, "lines"),
    #     unit(c(1, 1), "strwidth", list("0", "1")),
    #     unit(c(1, 1), "strheight", list("0", "1")),
    #     gp=gpar(col=NULL, fill="white"))
    grid.text(c(0, 1), 
              c(0, 1),
              unit(c(1, 1), "npc") + unit(1, "lines"))
    grid.lines(c(0, 0), unit(1, "npc") - unit(c(1, 4), "lines"))
    grid.segments(c(0, 0), 
                  unit(1, "npc") - unit(c(1, 4), "lines"), 
                  unit(c(-.5, -.5), "lines"),
                  unit(1, "npc") - unit(c(1, 4), "lines"))
    grid.text(c("0 lines", "3 lines"),
              unit(c(-1, -1), "lines"),
              unit(1, "npc") - unit(c(1, 4), "lines"),
              just=c("right", "bottom"))
    popViewport(2)

    popViewport()
}

figure3.6 <- function() {
    par(mar=rep(0, 4), cex=0.7)
    plot.new()
    plot.window(c(0.05, 0.95), 0:1)
    family <- c("sans", "serif", "mono")
    face <- 1:4
    for (i in 1:4)
        for (j in 1:3) {
            par(family=family[j], lheight=1.5)
            text(seq(.15, .85, length=4)[i],
                 seq(.25, .75, length=3)[j],
                 paste("family=\"", family[j], "\"\nfont=", face[i], sep=""),
                 font=face[i])
        }
    segments(.02, c(.125, .375, .625, .875), 
             .98, c(.125, .375, .625, .875), col="gray")
    segments(.02, c(.125, .375, .625, .875) - .01, 
             .02, c(.125, .375, .625, .875) + .01, col="gray")
    segments(.98, c(.125, .375, .625, .875) - .01, 
             .98, c(.125, .375, .625, .875) + .01, col="gray")
    rect(c(.27, .5, .73) - .01,
         .1,
         c(.27, .5, .73) + .01,
         .9, col="white", border=NA)
}

figure3.7 <- function() {
    par(mar=rep(0, 4), xaxs="i", yaxs="i", cex=0.8)
    plot.new()
    par(new=TRUE)
    grid.rect(gp=gpar(col="gray"))
    ncol <- 4
    nrow <- 4
    xadj <- c(1, 0.5, NA, 0)
    yadj <- c(1, 0.5, NA, 0)
    size <- unit(3, "mm")
    for (i in 1:nrow) {
        for (j in 1:ncol) {
            x <- i/(nrow + 1)
            y <- j/(ncol + 1)
            xu <- unit(x, "npc")
            yu <- unit(y, "npc")
            grid.segments(unit.c(xu - size, xu),
                          unit.c(yu, yu - size),
                          unit.c(xu + size, xu),
                          unit.c(yu, yu + size),
                          gp=gpar(col="gray"))
            text(x, y, paste("c(", xadj[j], ", ", yadj[i], ")", sep=""),
                 adj=c(xadj[j], yadj[i]))
        }
    }
}

figure3.8 <- function() {
    ncol <- 6
    nrow <- 1
    grid.rect(gp=gpar(col="gray"))
    for (i in 1:nrow) {
        for (j in 1:ncol) {
            x <- unit(j/(ncol+1), "npc")
            y <- unit(i/(nrow + 1), "npc")
            pch <- (i - 1)*ncol + j - 1
            grid.points(x + unit(3, "mm"), y, 
                        pch=pch, gp=gpar(fill="gray"))
            grid.text(pch, x - unit(3, "mm"), y, gp=gpar(col="gray"))
        }
    }
}

figure3.9 <- function() {
    x <- -5:5
    y <- -x^2 + 25
    plottype <- function(type) {
        par(mar=c(1, 0, 1, 0), pty="s")
        plot.new()
        plot.window(c(-6, 6), c(-2, 27))
        box(col="gray")
        points(x, y, type=type)
        mtext(paste("type=\"", type, "\"", sep=""))
    }

    par(mfrow=c(3, 2))
    plottype("p")
    plottype("l")
    plottype("b")
    plottype("o")
    plottype("h")
    plottype("s")
}

figure3.10 <- function() {
    axisfun <- function(mgp=c(3, 1, 0), xaxs="r", tcl=-.5,
                        mgpcol="black", xaxscol="black", tclcol="black") {
        par(mar=c(5, 1, 0, 1), mgp=mgp, xaxs=xaxs, tcl=tcl, pty="s")
        plot.new()
        box(col="gray")
        text(.5, .75, paste("mgp=c(", paste(mgp, collapse=", "), ")", sep=""),
             col=mgpcol)
        text(.5, .5, paste("xaxs=\"", xaxs, "\"", sep=""),
             col=xaxscol)
        text(.5, .25, paste("tcl=", tcl, sep=""),
             col=tclcol)
        axis(1, at=c(0, .5, 1))
        title(xlab="X-axis Label")
    }

    par(mfrow=c(2, 2))
    axisfun()
    axisfun(mgp=c(2, 0.3, 0), tcl=0.2, xaxscol="gray")
    axisfun(xaxs="i", mgpcol="gray", tclcol="gray")

}

figure3.11 <- function() {
    par(oma=rep(3, 4))
    vps <- gridBase::baseViewports()
    # Annotation helper function
    annWidth <- function(x, y, lab, above=TRUE, horiz=TRUE) {
        grid.lines(x=x, y=y, 
                   arrow=arrow(ends="both", angle=10, type="closed",
                               length=unit(3, "mm")), 
                   gp=gpar(fill="black"))
        nl <- length(lab)
        if (nl > 1) {
            y <- y + unit(c(-0.5, 0.5), "lines")
            if (horiz) {
                vjust <- 1:0
                hjust <- 0.5
                rot <- 0
            } else {
                hjust <- 1:0
                vjust <- 0.5
                rot <- 90
            }
        } else {
            hjust <- 0.5
            rot <- 0
            if (above) {
                y <- y + unit(0.5, "lines")
                vjust <- 0
            } else {
                y <- y - unit(0.5, "lines")
                vjust <- 1
            }
        }
        grid.text(lab,
                  x=0.5*sum(x),
                  y=y, hjust=hjust, vjust=vjust, rot=rot,
                  gp=gpar(fontfamily="mono", cex=1))
    }
    # Annotate whole page
    grid.rect(gp=gpar(col="gray", fill="gray80"))
    annWidth(0:1, unit(1, "npc") - unit(1.5, "lines"), "din[1]")
    # grid.lines(x=0.5)
    annWidth(unit(c(0, 3), "lines"), unit(0.7, "npc"), c("omi[2]", "oma[2]"))
    annWidth(unit(1, "npc") - unit(c(0, 3), "lines"),
             unit(0.7, "npc"), c("omi[4]", "oma[4]"))
    annWidth(unit(c(0, 3), "lines"), unit(0.3, "npc"), 
             "omd[1]", above=FALSE)
    annWidth(unit.c(unit(0, "npc"),
                    unit(1, "npc") - unit(3, "lines")),
             unit(2, "lines"), "omd[2]",
             above=FALSE)
    # Annotate figure region
    pushViewport(do.call("vpStack", vps[1:2]))
    grid.rect(gp=gpar(fill="gray90"))
    annWidth(0:1, unit(1, "npc") - unit(1.5, "lines"), "fin[1]")
    annWidth(unit(c(0, 4.1), "lines"), unit(0.6, "npc"), c("mai[2]", "mar[2]"))
    annWidth(unit(1, "npc") - unit(c(0, 2.1), "lines"),
             unit(0.6, "npc"), c("mai[4]", "mar[4]"), horiz=FALSE)
    annWidth(unit(c(0, 4.1), "lines"), unit(0.4, "npc"), 
             "plt[1]", above=FALSE)
    annWidth(unit.c(unit(0, "npc"),
                    unit(1, "npc") - unit(2.1, "lines")),
             unit(4, "lines"), "plt[2]",
             above=FALSE)
    # Annotate plot region
    pushViewport(vps[[3]])
    grid.rect(gp=gpar(lty="dashed", fill="gray80"))
    annWidth(0:1, unit(1, "npc") - unit(1.5, "lines"), "pin[1]")
    popViewport(3)
}

figure3.12 <- function() {
    grid.lshow <- function(i, j, lab, order, nrow, ncol, heights, respect) {
        pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
        pushViewport(viewport(width=unit(1, "npc") - unit(2, "lines"),
                              height=unit(1, "npc") - unit(3, "lines"),
                              y=unit(3, "lines"), just="bottom", 
                              layout=grid.layout(nrow, ncol, heights=heights, 
                                                 respect=respect)))
        grid.rect(gp=gpar(col="gray"))
        for (i in 1:nrow) {
            for (j in 1:ncol) {
                pushViewport(viewport(layout.pos.row=i, layout.pos.col=j))
                grid.rect()
                grid.text(order[i, j])
                popViewport()
            }
        }
        popViewport()
        grid.text(lab, y=unit(2, "lines"))
        popViewport()
    }
    pushViewport(viewport(layout=grid.layout(2, 2)))
    grid.lshow(1, 1, "(a)", cbind(c(1, 3, 5), c(2, 4, 6)), 3, 2, rep(1, 3), 
               FALSE)
    grid.lshow(1, 2, "(b)", cbind(c(6, 4, 2), c(5, 3, 1)), 3, 2, rep(1, 3), 
               FALSE)
    grid.lshow(2, 1, "(c)", matrix(c(1, 2), ncol=1), 2, 1, c(2, 1), FALSE)
    grid.lshow(2, 2, "(d)", matrix(c(1, 2), ncol=1), 2, 1, c(2, 1), TRUE)
    popViewport()

}

figure3.13 <- function() {
    grid.lshow <- function(i, j, lab, locs, nrow, ncol, heights, respect) {
        pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
        pushViewport(viewport(width=unit(1, "npc") - unit(2, "lines"),
                              height=unit(1, "npc") - unit(3, "lines"),
                              y=unit(3, "lines"), just="bottom", 
                              layout=grid.layout(nrow, ncol, heights=heights, 
                                                 respect=respect)))
        grid.rect(gp=gpar(col="gray"))
        for (i in locs) {
            pushViewport(viewport(layout.pos.row=i$rows, layout.pos.col=i$cols))
            grid.rect()
            grid.text(i$lab)
            popViewport()
        }
        popViewport()
        grid.text(lab, y=unit(2, "lines"))
        popViewport()
    }
    pushViewport(viewport(layout=grid.layout(2, 2)))
    grid.lshow(1, 1, "(a)", 
               list(
                   list(rows=1, cols=1, lab=1),
                   list(rows=3, cols=1, lab=2)),
               3, 1,
               unit(c(2, 0.5, 1), c("null", "cm", "null")), 
               TRUE)
    grid.lshow(1, 2, "(b)", 
               list(
                   list(rows=1, cols=1, lab=1),
                   list(rows=3, cols=1:2, lab=2),
                   list(rows=1, cols=2, lab=3)), 
               3, 2,
               unit(c(2, 0.5, 1), c("null", "cm", "null")), 
               TRUE)
    grid.lshow(2, 1, "(c)", 
               list(
                   list(rows=1, cols=1, lab=1),
                   list(rows=3, cols=1:2, lab=2),
                   list(rows=1, cols=2, lab=3)), 
               3, 2,
               unit(c(2, 0.5, 1), c("null", "cm", "null")), 
               cbind(c(0, 0, 1), c(0, 0, 0)))
    popViewport()

}

figure3.14 <- function() {
    par(mfrow=c(1, 2), mar=c(1, 1, 2, 1))
    par(cex=0.7)
    x <- 1:10
    y <- matrix(sort(rnorm(30)), ncol=3)
    plot(x, y[,1], ylim=range(y), ann=FALSE, axes=FALSE, 
         type="l", col="gray")
    box(col="gray")

    points(x, y[,1])
    lines(x, y[,2], col="gray")
    points(x, y[,2], pch=2)
    lines(x, y[,3], col="gray")
    points(x, y[,3], pch=3)

    mtext("points() & lines()", side=3, line=0.5)
    x <- 1:5
    y <- x
    plot(x, y, ann=FALSE, axes=FALSE, col="gray", pch=16)
    box(col="gray")

    text(x[-3], y[-3], c("right", "top", "bottom", "left"), 
         pos=c(4, 3, 1, 2))
    text(3, 3, "overlay")

    mtext("text()", side=3, line=0.5)

}

figure3.15 <- function() {
    t <- seq(60, 360, 30)
    x <- cos(t/180*pi)*t/360
    y <- sin(t/180*pi)*t/360

    source(system.file("extra", "as.raster.R", package="RGraphics"))
    rlogo <- pixmap::read.pnm(system.file("pictures/logo.pgm", 
                                          package="pixmap")[1])

    par(mfrow=c(1, 2), mar=c(1, 1, 2, 1))
    par(cex=0.7)

    t <- seq(60, 360, 30)
    x <- cos(t/180*pi)*t/360
    y <- sin(t/180*pi)*t/360

    par(mfrow=c(3, 3), mar=rep(1, 4), pty="s")
    plot(x, y, pch=16, col="gray",
         xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
         axes=FALSE, ann=FALSE)
    box(col="gray")
    mtext("lines()", side=3, line=.6, cex=.7, family="mono")
    lines(x, y)

    plot(x, y, pch=16, col="gray",
         xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
         axes=FALSE, ann=FALSE)
    box(col="gray")
    mtext("segments()", side=3, line=.6, cex=.7, family="mono")
    segments(0, 0, x, y)

    plot(x, y, pch=16, col="gray",
         xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
         axes=FALSE, ann=FALSE)
    box(col="gray")
    mtext("arrows()", side=3, line=.6, cex=.7, family="mono")
    arrows(0, 0, x[-1], y[-1], length=.1)

    plot(x, y, pch=16, col="gray",
         xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
         axes=FALSE, ann=FALSE)
    box(col="gray")
    mtext("xspline()", side=3, line=.6, cex=.7, family="mono")
    xspline(x, y, shape=1)

    plot(x, y, pch=16, col="gray",
         xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
         axes=FALSE, ann=FALSE)
    box(col="gray")
    mtext("rect()", side=3, line=.6, cex=.7, family="mono")
    rect(min(x), min(y), max(x), max(y), col="gray")

    plot(x, y, pch=16, col="gray",
         xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
         axes=FALSE, ann=FALSE)
    box(col="gray")
    mtext("polygon()", side=3, line=.6, cex=.7, family="mono")
    polygon(x, y, col="gray")

    plot(x, y, pch=16, col="gray",
         xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
         axes=FALSE, ann=FALSE)
    box(col="gray")
    mtext("polypath()", side=3, line=.6, cex=.7, family="mono")
    polypath(c(x, NA, .5*x), c(y, NA, .5*y),
             col="gray", rule="evenodd")

    plot(x, y, pch=16, col="gray",
         xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
         axes=FALSE, ann=FALSE)
    box(col="gray")
    mtext("xspline()", side=3, line=.6, cex=.7, family="mono")
    xspline(x, y, shape=1, open=FALSE, col="gray")

    plot(x, y, pch=16, col="gray",
         xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
         axes=FALSE, ann=FALSE)
    box(col="gray")
    mtext("rasterImage()", side=3, line=.6, cex=.7, family="mono")
    rasterImage(rlogo,
                x - .07, y - .07,
                x + .07, y + .07,
                interpolate=FALSE)
}

figure3.16 <- function() {
    par(mfrow=c(1, 2), mar=c(1, 1, 2, 1), pty="s")
    par(cex=0.7)
    x <- runif(20, 1, 10)
    y <- x + rnorm(20)
    plot(x, y, ann=FALSE, axes=FALSE, col="gray", pch=16)
    box(col="gray")

    lmfit <- lm(y ~ x)
    abline(lmfit)
    arrows(5, 8, 7, predict(lmfit, data.frame(x=7)),
           length=0.1)
    text(5, 8, "Line of best fit", pos=2)

    mtext("abline() & arrows()", side=3, line=0.5)
    y <- rnorm(50)
    hist(y, main="", xlab="", ylab="", axes=FALSE, 
         border="gray", col="light gray")
    box(col="gray")
    rug(y, ticksize=0.02)

    mtext("rug()", side=3, line=0.5)

}

figure3.17 <- function() {
    angle <- seq(0, 2*pi, length=13)[-13]
    x <- 0.15*cos(angle)
    y <- 0.5 + 0.3*sin(angle)
    par(mar=rep(0, 4))
    plot.new()
    box("outer", col="gray")
    polygon(0.25 + x, y, col="gray")
    text(0.75 + x[c(1, 5, 9)], y[c(1, 5, 9)], "NA", col="gray")
    x[c(1, 5, 9)] <- NA
    y[c(1, 5, 9)] <- NA
    polygon(0.75 + x, y, col="gray")

}

figure3.18 <- function() {
    par(mar=c(2, 1, 1, 1))
    y1 <- rnorm(100)
    y2 <- rnorm(100)

    par(mfrow=c(2, 1), xpd=NA)

    plot(y1, type="l", axes=FALSE,
         xlab="", ylab="", main="")
    box(col="gray")
    mtext("Left end of margin", adj=0, side=3)
    lines(x=c(20, 20, 40, 40), y=c(-7, max(y1), max(y1), -7), 
          lwd=3, col="gray")

    plot(y2, type="l", axes=FALSE,
         xlab="", ylab="", main="")
    box(col="gray")
    mtext("Right end of margin", adj=1, side=3)
    mtext("Label below x=30", at=30, side=1)
    lines(x=c(20, 20, 40, 40), y=c(7, min(y2), min(y2), 7), 
          lwd=3, col="gray")

}

figure3.19 <- function() {
    par(mfrow=c(2, 1), mar=c(5, 3, 2, 1), cex=0.5, pty="s")
    with(iris,
         plot(Sepal.Length, Sepal.Width, 
              pch=as.numeric(Species), cex=1.2))
    legend(6.1, 4.4, c("setosa", "versicolor", "virginica"), 
           cex=1.5, pch=1:3)

    barplot(VADeaths[1:2,], angle=c(45, 135), density=20, 
            col="gray", names=c("RM", "RF", "UM", "UF"))
    legend(0.4, 38, c("55-59", "50-54"), cex=1.5,
           angle=c(135, 45), density=20, fill="gray")

}

figure3.20 <- function() {
    par(cex=0.8)
    x <- 1:2
    y <- runif(2, 0, 100)
    par(mar=c(4, 4, 2, 4))
    plot(x, y, type="n", xlim=c(0.5, 2.5), ylim=c(-10, 110),
         axes=FALSE, ann=FALSE)

    axis(2, at=seq(0, 100, 20))
    mtext("Temperature (Centigrade)", side=2, line=3)

    axis(1, at=1:2, labels=c("Treatment 1", "Treatment 2"))
    axis(4, at=seq(0, 100, 20), labels=seq(0, 100, 20)*9/5 + 32)
    mtext("Temperature (Fahrenheit)", side=4, line=3)
    box()

    segments(x, 0, x, 100, lwd=20)
    segments(x, 0, x, 100, lwd=16, col="white")
    segments(x, 0, x, y, lwd=16, col="gray")

}

figure3.21 <- function() {
    par(mar=rep(1, 4))
    plot(0:1, 0:1, type="n", axes=FALSE, ann=FALSE)
    usr <- par("usr")
    pin <- par("pin")
    xcm <- diff(usr[1:2])/(pin[1]*2.54)
    ycm <- diff(usr[3:4])/(pin[2]*2.54)

    par(xpd=NA)
    rect(0 + 0.2*xcm, 0 - 0.2*ycm,
         1 + 0.2*xcm, 1 - 0.2*ycm,
         col="gray", border=NA)

    rect(0, 0, 1, 1, col="white")
    segments(seq(1, 8, 0.1)*xcm, 0,
             seq(1, 8, 0.1)*xcm, 
             c(rep(c(0.5, rep(0.25, 4), 
                     0.35, rep(0.25, 4)),
                   7), 0.5)*ycm)
    text(1:8*xcm, 0.6*ycm, 0:7, adj=c(0.5, 0))
    text(8.2*xcm, 0.6*ycm, "cm", adj=c(0, 0))

}

figure3.22 <- function() {
    layout(matrix(1:2, ncol=1), heights=1:2/6.5)
    par(cex=0.7)
    drunkenness <- ts(c(3875, 4846, 5128, 5773, 7327, 
                        6688, 5582, 3473, 3186,
                        rep(NA, 51)),
                      start=1912, end=1971)

    # Have to copy-and-paste to shrink the mtext text (arggh!)
    par(mar=c(5, 6, 2, 4))
    plot(drunkenness, lwd=3, col="gray", ann=FALSE, las=2)
    mtext("Drunkenness\nRelated Arrests", side=2, line=3.5, cex=0.7)
    par(new=TRUE)
    plot(nhtemp, ann=FALSE, axes=FALSE)
    mtext("Temperature (F)", side=4, line=3, cex=0.7)
    title("Using par(new=TRUE) or par(usr=...)")
    axis(4)

    par(mar=c(5, 4, 4, 2))
    with(trees, 
         {
             plot(Height, Volume, pch=3,
                  xlab="Height (ft)", 
                  ylab=expression(paste("Volume ", (ft^3))))
             symbols(Height, Volume, circles=Girth/12, 
                     fg="gray", inches=FALSE, add=TRUE)
         })

    mtext("symbols(..., add=TRUE)", font=2, side=3, line=1)

}

figure3.23 <- function() {
    xx <- c(1:50)
    yy <- rnorm(50)
    n <- 50
    hline <- 0

    xx <- c(1:50)
    yy <- rnorm(50)
    n <- 50
    hline <- 0

    par(mfrow=c(2,2), mar=c(3, 3, 1, 1))
    plot (yy ~ xx, type="n", axes=FALSE, ann=FALSE)
    polygon(c(xx[1], xx, xx[n]), c(min(yy), yy, min(yy)), 
            col="gray", border=NA)

    box(col="gray")
    plot (yy ~ xx, type="n", axes=FALSE, ann=FALSE)
    polygon(c(xx[1], xx, xx[n]), c(min(yy), yy, min(yy)), 
            col="gray", border=NA)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], hline, col="white", border=NA)

    box(col="gray")
    plot (yy ~ xx, type="n", axes=FALSE, ann=FALSE)
    polygon(c(xx[1], xx, xx[n]), c(min(yy), yy, min(yy)), 
            col="gray", border=NA)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], hline, col="white", border=NA)

    lines(xx, yy)

    box(col="gray")
    plot (yy ~ xx, type="n", axes=FALSE, ann=FALSE)
    polygon(c(xx[1], xx, xx[n]), c(min(yy), yy, min(yy)), 
            col="gray", border=NA)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], hline, col="white", border=NA)

    lines(xx, yy)

    abline (h=hline,col="gray")
    box()
    axis(1)
    axis(2) 

}

figure3.24 <- function() {
    par(mfrow=c(1, 2), mar=c(3, 3, 1, 1), cex=0.7)
    y <- sample(1:10)
    midpts <- barplot(y, col=" light gray")
    width <- diff(midpts[1:2])/4
    left <- rep(midpts, y - 1) - width
    right <- rep(midpts, y - 1) + width
    heights <- unlist(apply(matrix(y, ncol=10), 
                            2, seq))[-cumsum(y)]
    segments(left, heights, right, heights,
             col="white")

    with(ToothGrowth, 
         {
             boxplot(len ~ supp, border="gray", 
                     col="light gray", boxwex=0.5)
             points(jitter(rep(1:2, each=30), 0.5), 
                    unlist(split(len, supp)),
                    cex=0.5, pch=16)
         })

}

figure3.25 <- function() {
    par(cex=.7)
    pairs(iris[1:2], 
          diag.panel=function(x, ...) { 
              boxplot(x, add=TRUE, axes=FALSE,
                      at=mean(par("usr")[1:2])) 
          }, 
          text.panel=function(x, y, labels, ...) { 
              mtext(labels, side=3, line=0) 
          })

}

figure3.26 <- function() {
    par(mar=rep(0, 4))
    z <- 2 * volcano    
    x <- 10 * (1:nrow(z))   
    y <- 10 * (1:ncol(z))   
    trans <- persp(x, y, z, zlim=c(0, max(z)),
                   theta = 150, phi = 12, lwd=.5,
                   scale = FALSE, axes=FALSE)

    clines <- contourLines(x, y, z)
    lapply(clines,
           function(contour) {
               lines(trans3d(contour$x, contour$y, 0, trans))
           })
}

figure3.27 <- function() {
    groups <- dimnames(Titanic)[[1]]
    males <- Titanic[, 1, 2, 2]
    females <- Titanic[, 2, 2, 2]

    par(mar=c(0.5, 3, 0.5, 1))

    plot.new()
    plot.window(xlim=c(-200, 200), ylim=c(-1.5, 4.5))

    ticks <- seq(-200, 200, 100)
    y <- 1:4
    h <- 0.2

    lines(rep(0, 2), c(-1.5, 4.5), col="gray")
    segments(-200, y, 200, y, lty="dotted")
    rect(-males, y-h, 0, y+h, col="dark gray")
    rect(0, y-h, females, y+h, col="light gray")
    mtext(groups, at=y, adj=1, side=2, las=2)
    par(cex.axis=0.5, mex=0.5)
    axis(1, at=ticks, labels=abs(ticks), pos=0)

    tw <- 1.5*strwidth("females")
    rect(-tw, -1-h, 0, -1+h, col="dark gray")
    rect(0, -1-h, tw, -1+h, col="light gray")
    text(0, -1, "males", pos=2)
    text(0, -1, "females", pos=4)

    box("inner", col="gray")
}
