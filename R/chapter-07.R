figure7.1 <- function() {
    grid.rect(gp=gpar(col="gray"))
    grid.circle(name="circles", x=seq(0.1, 0.9, length=40), 
                y=0.5 + 0.4*sin(seq(0, 2*pi, length=40)),
                r=abs(0.1*cos(seq(0, 2*pi, length=40))))

    grid.edit("circles", 
              gp=gpar(col=gray(c(1:20*0.04, 20:1*0.04))))
    grid.remove("circles")
}

figure7.2 <- function() {
    grid.rect(gp=gpar(col="gray"))
    suffix <- c("even", "odd")
    for (i in 1:8)
        grid.circle(name=paste("circle.", suffix[i %% 2 + 1], 
                               sep=""),
                    r=(9 - i)/20, 
                    gp=gpar(col=NA, fill=gray(i/10)))

    grid.edit("circle.odd", gp=gpar(fill="gray10"), 
              global=TRUE)

    grid.edit("circle", gp=gpar(col="gray", fill="gray90"), 
              grep=TRUE, global=TRUE) 
}

figure7.3 <- function() {
    labels <- c("\"xaxis1\"\nxaxis gTree", "\"major\"\nlines grob", 
                "\"ticks\"\nlines grob", "\"labels\"\ntext grob")
    names <- c("", "major", "ticks", "labels")
    boxheight <- unit(2.5, "line")
    boxwidth <- unit(1.2, "in")
    pushViewport(viewport(layout=grid.layout(2, 3)))
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
    grid.text(labels[1])
    grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
               0.5, gp=gpar(col="gray"))
    grid.roundrect(height=boxheight, 
                   width=boxwidth, # 1.2*stringWidth(labels[1]),
                   r=unit(2, "mm"),
                   gp=gpar(fill=NA))
    popViewport()
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
    grid.text(labels[2])
    grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
               0.5, gp=gpar(col="gray"))
    grid.roundrect(height=boxheight, 
                   width=boxwidth, # 1.2*stringWidth(labels[2]),
                   r=unit(2, "mm"),
                   gp=gpar(fill=NA))
    popViewport()
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
    grid.text(labels[3])
    grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
               0.5, gp=gpar(col="gray"))
    grid.roundrect(height=boxheight, 
                   width=boxwidth, # 1.2*stringWidth(labels[3]),
                   r=unit(2, "mm"),
                   gp=gpar(fill=NA))
    popViewport()
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=3))
    grid.text(labels[4])
    grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
               0.5, gp=gpar(col="gray"))
    grid.roundrect(height=boxheight, 
                   width=boxwidth, # 1.2*stringWidth(labels[4]),
                   r=unit(2, "mm"),
                   gp=gpar(fill=NA))
    popViewport()
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
    grid.move.to(x=0.5, y=unit(0.5, "npc") - 0.5*boxheight)
    popViewport()
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
    grid.line.to(x=0.5, y=unit(0.5, "npc") + 0.5*boxheight,
                 arrow=arrow(angle=10, length=unit(3, "mm")))
    popViewport()
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
    grid.move.to(x=0.5, y=unit(0.5, "npc") - 0.5*boxheight)
    popViewport()
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
    grid.line.to(x=0.5, y=unit(0.5, "npc") + 0.5*boxheight,
                 arrow=arrow(angle=10, length=unit(3, "mm")))
    popViewport()
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
    grid.move.to(x=0.5, y=unit(0.5, "npc") - 0.5*boxheight)
    popViewport()
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=3))
    grid.line.to(x=0.5, y=unit(0.5, "npc") + 0.5*boxheight,
                 arrow=arrow(angle=10, length=unit(3, "mm")))
    popViewport()
}

figure7.4 <- function() {
    grid.rect(gp=gpar(col="gray"))
    pushViewport(viewport(just="bottom", gp=gpar(cex=0.7)))
    grid.xaxis(name="axis1", at=1:4/5)
    grid.ls()

    grid.edit("axis1", at=1:3/4)

    grid.edit(gPath("axis1", "labels"), rot=45)

    popViewport()
}

figure7.5 <- function() {
    tg <- textGrob("sample text")
    rg <- rectGrob(width=1.1*grobWidth(tg), 
                   height=1.3*grobHeight(tg))
    boxedText <- gTree(children=gList(tg, rg))

    pushViewport(viewport(layout=grid.layout(1, 7,
                                             heights=unit(1.25, "in"),
                                             widths=unit(rep(c(1, 1.25), length=7),
                                                         rep(c("null", "in"),
                                                             length=7)))))
    pushViewport(viewport(layout.pos.col=2, gp=gpar(fill=NA)))
    grid.rect(gp=gpar(col="gray", fill=NA))
    grid.draw(boxedText)

    popViewport()
    pushViewport(viewport(layout.pos.col=4, gp=gpar(fill=NA)))
    grid.rect(gp=gpar(col="gray", fill=NA))
    grid.draw(editGrob(boxedText, gp=gpar(col="gray")))

    popViewport()
    pushViewport(viewport(layout.pos.col=6, gp=gpar(fill=NA)))
    grid.rect(gp=gpar(col="gray", fill=NA))
    grid.draw(editGrob(boxedText, vp=viewport(angle=45),
                       gp=gpar(fontsize=18)))

    popViewport()
    popViewport()

}
figure7.6 <- function() {
    label <- textGrob("A\nPlot\nLabel ",
                      x=0, just="left")
    x <- seq(0.1, 0.9, length=50)
    y <- runif(50, 0.1, 0.9)
    gplot <- 
        gTree(
            children=gList(rectGrob(gp=gpar(col="gray60",
                                            fill="white")),
                           linesGrob(x, y), 
                           pointsGrob(x, y, pch=16, 
                                      size=unit(1.5, "mm"))),
            vp=viewport(width=unit(1, "npc") - unit(5, "mm"), 
                        height=unit(1, "npc") - unit(5, "mm")))

    layout <- grid.layout(1, 2,
                          widths=unit(c(1, 1), 
                                      c("null", "grobwidth"),
                                      list(NULL, label)))

    grid.rect(gp=gpar(col="gray60", fill="gray90"))
    pushViewport(viewport(layout=layout))
    pushViewport(viewport(layout.pos.col=2))
    grid.draw(label)
    popViewport()
    pushViewport(viewport(layout.pos.col=1))
    grid.draw(gplot)
    popViewport(2)

}
figure7.7 <- function() {
    tg1 <- textGrob("Sample")
    rg1 <- rectGrob(x=rep(0.5, 2),
                    width=1.1*grobWidth(tg1), 
                    height=1.3*grobHeight(tg1),
                    gp=gpar(col=c("gray60", "white"), 
                            lwd=c(3, 1)))

    pushViewport(viewport(layout=grid.layout(1, 7,
                                             heights=unit(1.25, "in"),
                                             widths=unit(rep(c(1, 1.25), length=7),
                                                         rep(c("null", "in"),
                                                             length=7)))))
    pushViewport(viewport(layout.pos.col=2, gp=gpar(fill=NA)))
    grid.rect(gp=gpar(col="gray", fill=NA))
    grid.draw(tg1)
    grid.draw(rg1)

    popViewport()
    pushViewport(viewport(layout.pos.col=4, gp=gpar(fill=NA)))
    grid.rect(gp=gpar(col="gray", fill=NA))
    pushViewport(viewport(gp=gpar(cex=2)))
    grid.draw(tg1)
    grid.draw(rg1)
    popViewport()

    popViewport()
    pushViewport(viewport(layout.pos.col=6, gp=gpar(fill=NA)))
    grid.rect(gp=gpar(col="gray", fill=NA))
    pushViewport(viewport(gp=gpar(cex=2)))
    grid.draw(tg1)
    popViewport()
    grid.draw(rg1)

    popViewport()
    popViewport()
}

figure7.8 <- function() {
    tg1 <- textGrob("Sample", name="tg1")
    rg1 <- rectGrob(width=1.1*grobWidth("tg1"), 
                    height=1.3*grobHeight("tg1"),
                    gp=gpar(col="gray60", lwd=3))
    rg2 <- rectGrob(width=1.1*grobWidth(tg1), 
                    height=1.3*grobHeight(tg1),
                    gp=gpar(col="white"))

    grid.rect(gp=gpar(col="gray"))
    pushViewport(viewport(gp=gpar(cex=1.5, fill=NA)))
    grid.draw(tg1)
    grid.draw(rg1)
    grid.draw(rg2)

    grid.edit("tg1", grep=TRUE, global=TRUE, 
              label="Different text")

    popViewport()
}

figure7.9 <- function() {
    grid.rect(gp=gpar(col="gray"))
    pushViewport(viewport(gp=gpar(fill=NA)))
    grid.circle(.25, .5, r=unit(1, "mm"), 
                gp=gpar(fill="black"))
    grid.text("A label", .75, .5)
    grid.rect(.75, .5, 
              width=stringWidth("A label") + unit(2, "mm"),
              height=unit(1, "line"),
              name="labelbox")

    grid.segments(.25, .5, 
                  grobX("labelbox", 180), .5,
                  arrow=arrow(angle=15, type="closed"),
                  gp=gpar(fill="black"))
}

figure7.10 <- function() {
    pushViewport(viewport(gp=gpar(fill=NA)))
    vptop <- viewport(width=.9, height=.4, y=.75,
                      name="vptop")
    vpbot <- viewport(width=.9, height=.4, y=.25,
                      name="vpbot")
    pushViewport(vptop)
    upViewport()
    pushViewport(vpbot)
    upViewport()

    grid.rect(vp="vptop")
    grid.lines(1:50/51, runif(50), vp="vptop")
    grid.rect(vp="vpbot")
    grid.lines(1:50/51, runif(50), vp="vpbot")

    grid.null(x=.2, y=.95, vp="vptop", name="tl")
    grid.null(x=.4, y=.95, vp="vptop", name="tr")
    grid.null(x=.2, y=.05, vp="vpbot", name="bl")
    grid.null(x=.4, y=.05, vp="vpbot", name="br")

    grid.polygon(unit.c(grobX("tl", 0),
                        grobX("tr", 0),
                        grobX("br", 0),
                        grobX("bl", 0)),
                 unit.c(grobY("tl", 0),
                        grobY("tr", 0),
                        grobY("br", 0),
                        grobY("bl", 0)),
                 gp=gpar(col="gray", lwd=3))
}

figure7.11 <- function() {
    grid.rect(gp=gpar(col="gray"))
    grid.circle(r=0.3, gp=gpar(fill="gray80"), 
                name="mycircle")
    grid.edit("mycircle", gp=gpar(lwd=5))
    grid.edit("mycircle", gp=gpar(lty="dashed"))
}

figure7.12 <- function() {
    angle <- seq(0, 2*pi, length=21)[-21]
    x <- cos(angle)
    y <- sin(angle)

    trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
    print(
        xyplot(y ~ x, aspect=1, 
               xlab="displacement", 
               ylab="velocity")
    
    )
    grid.edit("[.]xlab$", grep=TRUE, 
              x=unit(1, "npc"), just="right",
              gp=gpar(fontfamily="mono"))
    grid.edit("[.]ylab$", grep=TRUE, 
              y=unit(1, "npc"), just="right",
              gp=gpar(fontfamily="mono"))
}

figure7.13 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    update_geom_defaults("smooth", aes(color="black"))
    print(
        ggplot(mtcars2, aes(x=disp, y=mpg)) +
            geom_point() +
            geom_smooth(method=lm)
    
    )
    downViewport("panel.3-4-3-4")
    sline <- grid.get(gPath("smooth", "polyline"),
                      grep=TRUE)
    grid.segments(.7, .8, 
                  grobX(sline, 45), grobY(sline, 45),
                  arrow=arrow(angle=10, type="closed"),
                  gp=gpar(fill="black"))
    grid.text("line of best fit", .71, .81,
              just=c("left", "bottom"))
}
