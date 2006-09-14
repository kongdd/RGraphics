figureB.1 <- function() {
midpts <- barplot(1:10, col="grey90", axes=FALSE)
axis(2)
axis(1, at=midpts, labels=FALSE)

library(gridBase)
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(c("one", "two", "three", "four", "five",
            "six", "seven", "eight", "nine", "ten"), 
          x=unit(midpts, "native"), y=unit(-1, "lines"),
          just="right", rot=60)
popViewport(3)




}
figureB.2 <- function() {
hc <- hclust(dist(USArrests), "ave")
dend1 <- as.dendrogram(hc)
dend2 <- cut(dend1, h=70)



x <- 1:4
y <- 1:4
height <- factor(round(unlist(lapply(dend2$lower, 
                                     attr, "height"))))



space <- 1.2 * max(unit(rep(1, 50), "strwidth",
                        as.list(rownames(USArrests))))
dendpanel <- function(x, y, subscripts, ...) {
  pushViewport(viewport(gp=gpar(fontsize=8)),
               viewport(y=unit(0.95, "npc"), width=0.9,
                        height=unit(0.95, "npc") - space,
                        just="top"))
  par(plt=gridPLT(), new=TRUE, ps=8)
  plot(dend2$lower[[subscripts]], axes=FALSE)
  popViewport(2)
}



trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
library(lattice)
plot.new()
print(xyplot(y ~ x | height, subscripts=TRUE, 
             xlab="", ylab="",
             strip=function(...) { 
               strip.default(style=4, ...) 
             },
             scales=list(draw=FALSE), 
             panel=dendpanel),
      newpage=FALSE)




}
figure3.1 <- function() {
par(oma=rep(3, 4), bg="grey80")
plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
box("outer", col="grey")
# set clipping to figure region
par(xpd=TRUE)
# deliberately draw a stupidly large rectangle
rect(-1, -1, 2, 2, col="grey90")
box("figure")
# set clipping back to plot region
par(xpd=FALSE)
# deliberately draw a stupidly large rectangle
rect(-1, -1, 2, 2, col="grey80")
box("plot", lty="dashed")
text(.5, .5, "Plot Region")
mtext("Figure Region", side=3, line=2)
for (i in 1:4)
    mtext(paste("Outer margin", i), side=i, line=1, outer=TRUE)



}
figure3.2 <- function() {
par(oma=rep(3, 4), mfrow=c(3,2), bg="grey80")
for (i in 1:6) {
    if (i == 3) {
      omar <- par(mar=c(2, 2, 2, 1))  
      plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
      par(xpd=TRUE)
      rect(-1, -1, 2, 2, col="grey90")
      box("figure")
      par(xpd=FALSE)
      rect(-1, -1, 2, 2, col="grey80")
      box("plot", lty="dashed")
      text(.5, .5, "Current Plot Region", cex=1.5)
      mtext("Current Figure Region", side=3)
      par(omar)
    } else {
      omar <- par(mar=rep(0, 4))  
      plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
      par(xpd=TRUE)
      rect(-1, -1, 2, 2, col="grey90")
      box("figure")
      text(.5, .5, paste("Figure", i), cex=1.5)
      par(omar)
    }
}
box("outer", col="grey")
for (i in 1:4)
    mtext(paste("Outer margin", i), side=i, line=1, outer=TRUE)



}
figure3.3 <- function() {
par(xaxs="i", yaxs="i", xpd=FALSE)
    plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
    box("figure")
    rect(0, 0, 1, 1, col="light grey", border="grey")
    axis(1, at=c(0, 1), c("", ""))
    mtext("Min x value", side=1, adj=0, line=1)
    mtext("Max x value", side=1, adj=1, line=1)
    axis(2, at=c(0, 1), c("", ""))
    mtext("Min y value", side=2, adj=0, line=1)
    mtext("Max y value", side=2, adj=1, line=1)
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
grid.rect(gp=gpar(col="grey"))
grid.text("Current Plot", gp=gpar(col="grey"))
grid.rect(0, unit(-5, "lines"), 1, unit(5, "lines"),
          just=c("left", "bottom"), gp=gpar(col="grey", fill="light grey"))
grid.text("Figure\nMargin\n1", y=unit(-2.5, "lines"))
grid.lines(c(0, 1), c(0, 0))
grid.segments(c(0, 1), c(0, 0), c(0, 1), unit(c(.5, .5), "lines"))
grid.text(c("xmin", "xmax"), c(0, 1), unit(c(1, 1), "lines"))
grid.lines(c(0, 0), unit(c(-1, -4), "lines"))
grid.segments(c(0, 0), unit(c(-1, -4), "lines"), 
              unit(c(-.5, -.5), "lines"), unit(c(-1, -4), "lines"))
grid.text(c("0 lines", "3 lines"),
          unit(c(-1, -1), "lines"), unit(c(-1, -4), "lines"),
          just="right")
popViewport(2)
pushViewport(viewport(layout.pos.row=3))
grid.rect()
pushViewport(plotViewport(c(5, 5, 3, 2), yscale=c(0, 11)))
grid.rect(gp=gpar(col="grey"))
grid.text("Current Plot", gp=gpar(col="grey"))
grid.rect(unit(-5, "lines"), 0, unit(5, "lines"), 1,
          just=c("left", "bottom"), gp=gpar(col="grey", fill="light grey"))
grid.text("Figure\nMargin\n2", x=unit(-2.5, "lines"))
grid.lines(c(0, 0), c(0, 1))
grid.segments(c(0, 0), c(0, 1), unit(c(.5, .5), "lines"), c(0, 1))
grid.text(c("ymin", "ymax"), unit(c(1, 1), "lines"), c(0, 1), just="left")
grid.lines(unit(c(0, -3), "lines"), c(0, 0))
grid.segments(unit(c(0, -3), "lines"), c(0, 0), 
              unit(c(0, -3), "lines"), unit(c(-.5, -.5), "lines"))
grid.text(c("0 lines", "3 lines"),
          unit(c(0, -3), "lines"), unit(c(-1, -1), "lines"))
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
    grid.rect(gp=gpar(col="grey"))
      pushViewport(plotViewport(c(2, 2, 1, 1), xscale=c(0, 11),
        gp=gpar(col="grey")))
      grid.rect(gp=gpar(col="grey"))
      grid.text(paste("Plot", j - 1))
      popViewport()      
    popViewport()
  }
}
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
grid.rect(gp=gpar(col="grey", fill="light grey"))
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
          unit(c(-1, -1), "lines"))
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
    grid.rect(gp=gpar(col="grey"))
      pushViewport(plotViewport(c(2, 2, 1, 1), xscale=c(0, 11),
        gp=gpar(col="grey")))
      grid.rect(gp=gpar(col="grey"))
      grid.text(paste("Plot", j - 1))
      popViewport()      
    popViewport()
  }
}
pushViewport(viewport(layout.pos.row=3, layout.pos.col=2:3))
grid.rect(gp=gpar(col="grey", fill="light grey"))
grid.text("Outer Margin 1")
grid.lines(c(0, 1), c(1, 1))
grid.segments(c(0, 1), 
              unit(c(1, 1), "npc"),
	      c(0, 1),
              unit(c(1, 1), "npc") + unit(.5, "lines"))
grid.rect(c(0, 1), 
	  unit(c(1, 1), "npc") + unit(1, "lines"),
	  unit(c(1, 1), "strwidth", list("0", "1")),
	  unit(c(1, 1), "strheight", list("0", "1")),
	  gp=gpar(col=NULL, fill="white"))
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
          just="right")
popViewport(2)

popViewport()



}
figure3.6 <- function() {
names <- c("blank", "solid", "dashed", "dotted", "dotdash",
  "longdash", "twodash", "", "", "13", "F8", "431313", "22848222")
hgap <- unit(5, "mm")
pushViewport(viewport(layout=grid.layout(17, 5,
  widths=unit.c(unit(1, "strwidth", "Integer") + unit(3, "mm"), 
    hgap, unit(1.5, "inches"),
    hgap, max(unit(rep(1, length(names)), "strwidth", data=as.list(names))) + unit(3, "mm")),
  heights=unit(c(1.2, 1, 1, rep(1.2, 7), 1, 1, rep(1.2, 4), 1), "lines"))))
pushViewport(viewport(layout.pos.col=1:5, layout.pos.row=1:17))
grid.rect(width=1.3, height=1.3, gp=gpar(col="grey"))
popViewport()
for (i in 1:17) {
	if (i == 1) {
	  pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))  
	  grid.text("Integer", gp=gpar(fontface="bold"), just="right", x=1)
	  popViewport()
	  pushViewport(viewport(layout.pos.col=3, layout.pos.row=i))  
	  grid.text("Sample line", gp=gpar(fontface="bold"))
	  popViewport()
	  pushViewport(viewport(layout.pos.col=5, layout.pos.row=i))  
	  grid.text("String", gp=gpar(fontface="bold"), just="left", x=0)
	  popViewport()
	  pushViewport(viewport(layout.pos.row=i))  
	  grid.lines(c(0, 1), 0)
	  popViewport()
        } else if (i == 3) {
	    pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))  
	    grid.text("Predefined", just="left", x=0, 
              gp=gpar(fontface="italic"))
	    popViewport()          
        } else if (i == 12) {
	    pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))  
	    grid.text("Custom", just="left", x=0, 
              gp=gpar(fontface="italic"))
	    popViewport()          
	} else if ((i > 3 && i < 11) || 
                   (i > 12 && i < 17)) {
	  if (i < 11) {
	    pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))  
	    grid.text(i-4, just="right", x=1)
	    popViewport()
	  }
	  if (nchar(names[i-3])) {
		  pushViewport(viewport(layout.pos.col=5, layout.pos.row=i))  
		  grid.text(paste("\"", names[i-3], "\"", sep=""), x=0, just="left")
		  popViewport()
		  pushViewport(viewport(layout.pos.col=3, layout.pos.row=i))  
		  grid.lines(c(0, 1), 0.5, gp=gpar(lty=names[i-3], lwd=2))
		  popViewport()
	  }
        }
	if (i == 17) {
	    	  pushViewport(viewport(layout.pos.row=i))  
		  grid.lines(c(0, 1), 0)
		  popViewport()
	}
}
popViewport()



}
figure3.7 <- function() {
x <- c(.3, .7, .3)
y <- c(.2, .5, .8)
grid.rect(gp=gpar(col="grey"))
grid.lines(x, y, gp=gpar(lwd=40, lineend="square",
                   linejoin="mitre", col="black"))
grid.lines(x, y, gp=gpar(lwd=40, col="grey50"))
                   # lineend="round", linejoin="round"
grid.lines(x, y, gp=gpar(lwd=40, lineend="butt",
                   linejoin="bevel", col="grey80"))
grid.points(x, y, default="npc", pch=16, gp=gpar(cex=0.5))




}
figure3.8 <- function() {
par(mar=rep(0, 4), xaxs="i", yaxs="i", cex=0.8)
plot.new()
par(new=TRUE)
grid.rect(gp=gpar(col="grey"))
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
		  gp=gpar(col="grey"))
    text(x, y, paste("c(", xadj[j], ", ", yadj[i], ")", sep=""),
         adj=c(xadj[j], yadj[i]))
  }
}



}
figure3.9 <- function() {
# On my X11 server, with a UTF-8 locale, there were no 
# font sets for the symbol font, so I had to set the locale
# so that a single (symbol) font was searched for instead.
# Sys.setlocale(locale="en_NZ.iso88591")

par(mar=rep(0, 4), cex=0.7)
plot.new()
plot.window(c(0.05, 0.95), 0:1)
family <- c("sans", "serif", "mono", "symbol")
face <- 1:4
for (i in 1:4)
  for (j in 1:4) {
    par(family=family[j], lheight=1.5)
    text(seq(.15, .85, length=4)[i],
         seq(.15, .85, length=4)[j],
         paste("family=\"", family[j], "\"\nfont=", face[i], sep=""),
         font=face[i])
  }
segments(.02, c(.04, .27, .5, .73, .96), 
         .98, c(.04, .27, .5, .73, .96), col="grey")
segments(.02, c(.04, .27, .5, .73, .96) - .01, 
         .02, c(.04, .27, .5, .73, .96) + .01, col="grey")
segments(.98, c(.04, .27, .5, .73, .96) - .01, 
         .98, c(.04, .27, .5, .73, .96) + .01, col="grey")
rect(c(.27, .5, .73) - .01,
     .02,
     c(.27, .5, .73) + .01,
     .98, col="white", border=NA)



}
figure3.10 <- function() {
ncol <- 6
nrow <- 5
grid.rect(gp=gpar(col="grey"))
for (i in 1:nrow) {
  for (j in 1:ncol) {
    x <- unit(j/(ncol+1), "npc")
    y <- unit(i/(nrow + 1), "npc")
    pch <- (i - 1)*ncol + j - 1
    if (pch > 25) 
      pch <- c("A", "b", ".", "#")[pch - 25]
    grid.points(x + unit(3, "mm"), y, 
      pch=pch, gp=gpar(fill="grey"))
    grid.text(pch, x - unit(3, "mm"), y, gp=gpar(col="grey"))
  }
}



}
figure3.11 <- function() {
x <- -5:5
y <- -x^2 + 25
plottype <- function(type) {
  par(mar=c(1, 0, 1, 0), pty="s")
  plot.new()
  plot.window(c(-6, 6), c(-2, 27))
  box(col="grey")
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
figure3.12 <- function() {
axisfun <- function(mgp=c(3, 1, 0), xaxs="r", tcl=-.5,
                    mgpcol="black", xaxscol="black", tclcol="black") {
  par(mar=c(5, 1, 0, 1), mgp=mgp, xaxs=xaxs, tcl=tcl, pty="s")
  plot.new()
  box(col="grey")
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
axisfun(xaxs="i", mgpcol="grey", tclcol="grey")
axisfun(mgp=c(2, 0.3, 0), tcl=0.3, xaxscol="grey")



}
figure3.13 <- function() {
library(gridBase)
par(oma=rep(3, 4))
vps <- baseViewports()
# Annotation helper function
annWidth <- function(x, y, lab, above=TRUE, horiz=TRUE) {
  grid.arrows(x=x, y=y, 
              ends="both", angle=10, type="closed",
              length=unit(3, "mm"), gp=gpar(fill="black"))
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
grid.rect(gp=gpar(col="grey", fill="grey80"))
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
grid.rect(gp=gpar(fill="grey90"))
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
grid.rect(gp=gpar(lty="dashed", fill="grey80"))
annWidth(0:1, unit(1, "npc") - unit(1.5, "lines"), "pin[1]")
popViewport(3)



}
figure3.14 <- function() {
grid.lshow <- function(i, j, lab, order, nrow, ncol, heights, respect) {
  pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
  pushViewport(viewport(width=unit(1, "npc") - unit(2, "lines"),
               height=unit(1, "npc") - unit(3, "lines"),
	       y=unit(3, "lines"), just="bottom", 
    layout=grid.layout(nrow, ncol, heights=heights, 
      respect=respect)))
  grid.rect(gp=gpar(col="grey"))
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
figure3.15 <- function() {
grid.lshow <- function(i, j, lab, locs, nrow, ncol, heights, respect) {
  pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
  pushViewport(viewport(width=unit(1, "npc") - unit(2, "lines"),
               height=unit(1, "npc") - unit(3, "lines"),
	       y=unit(3, "lines"), just="bottom", 
    layout=grid.layout(nrow, ncol, heights=heights, 
      respect=respect)))
  grid.rect(gp=gpar(col="grey"))
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
figure3.16 <- function() {
par(mfrow=c(2, 2), mar=c(1, 1, 2, 1))
par(cex=0.7)
x <- 1:10
y <- matrix(sort(rnorm(30)), ncol=3)
plot(x, y[,1], ylim=range(y), ann=FALSE, axes=FALSE, 
     type="l", col="grey")
box(col="grey")

points(x, y[,1])
lines(x, y[,2], col="grey")
points(x, y[,2], pch=2)
lines(x, y[,3], col="grey")
points(x, y[,3], pch=3)


mtext("points() & lines()", side=3, line=0.5)
x <- c(4, 5, 2, 1)
y <- x
plot(x, y, ann=FALSE, axes=FALSE, col="grey", pch=16)
points(3, 3, col="grey", pch=16)
box(col="grey")

text(x, y, c("bottom", "left", "top", "right"), pos=1:4)
text(3, 3, "overlay")

mtext("text()", side=3, line=0.5)
x <- rnorm(100)
y <- rnorm(100)
plot(x, y, ann=FALSE, axes=FALSE, col="grey")
box(col="grey")

rect(min(x), min(y), max(x), max(y), lty="dashed")
hull <- chull(x, y)
polygon(x[hull], y[hull])

mtext("rect() & poly()", side=3, line=0.5)



}
figure3.17 <- function() {
par(mfrow=c(1, 2), mar=c(1, 1, 2, 1), pty="s")
par(cex=0.7)
x <- runif(20, 1, 10)
y <- x + rnorm(20)
plot(x, y, ann=FALSE, axes=FALSE, col="grey", pch=16)
box(col="grey")

lmfit <- lm(y ~ x)
abline(lmfit)
arrows(5, 8, 7, predict(lmfit, data.frame(x=7)),
       length=0.1)
text(5, 8, "Line of best fit", pos=2)

mtext("abline() & arrows()", side=3, line=0.5)
y <- rnorm(50)
hist(y, main="", xlab="", ylab="", axes=FALSE, 
     border="grey", col="light grey")
box(col="grey")
rug(y, ticksize=0.02)

mtext("rug()", side=3, line=0.5)



}
figure3.18 <- function() {
angle <- seq(0, 2*pi, length=13)[-13]
x <- 0.15*cos(angle)
y <- 0.5 + 0.3*sin(angle)
par(mar=rep(0, 4))
plot.new()
box("outer", col="grey")
polygon(0.25 + x, y, col="grey")
text(0.75 + x[c(1, 5, 9)], y[c(1, 5, 9)], "NA", col="grey")
x[c(1, 5, 9)] <- NA
y[c(1, 5, 9)] <- NA
polygon(0.75 + x, y, col="grey")




}
figure3.19 <- function() {
par(mar=c(2, 1, 1, 1))
y1 <- rnorm(100)
y2 <- rnorm(100)

par(mfrow=c(2, 1), xpd=NA)

plot(y1, type="l", axes=FALSE,
     xlab="", ylab="", main="")
box(col="grey")
mtext("Left end of margin", adj=0, side=3)
lines(x=c(20, 20, 40, 40), y=c(-7, max(y1), max(y1), -7), 
      lwd=3, col="grey")

plot(y2, type="l", axes=FALSE,
     xlab="", ylab="", main="")
box(col="grey")
mtext("Right end of margin", adj=1, side=3)
mtext("Label below x=30", at=30, side=1)
lines(x=c(20, 20, 40, 40), y=c(7, min(y2), min(y2), 7), 
      lwd=3, col="grey")




}
figure3.20 <- function() {
par(mfrow=c(2, 1), mar=c(5, 3, 2, 1), cex=0.5, pty="s")
with(iris,
     plot(Sepal.Length, Sepal.Width, 
          pch=as.numeric(Species), cex=1.2))
legend(6.1, 4.4, c("setosa", "versicolor", "virginica"), 
       cex=1.5, pch=1:3)

barplot(VADeaths[1:2,], angle=c(45, 135), density=20, 
        col="grey", names=c("RM", "RF", "UM", "UF"))
legend(0.4, 38, c("55-59", "50-54"), cex=1.5,
       angle = c(135, 45), density = 20, fill = "grey")




}
figure3.21 <- function() {
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

segments(x, 0, x, 100, lwd=20, col="dark grey")
segments(x, 0, x, 100, lwd=16, col="white")
segments(x, 0, x, y, lwd=16, col="light grey")




}
figure3.22 <- function() {
h <- 0.01
drawexpr <- function(expr, y) {
  grid.text(paste("expression(", expr, ")", sep=""), .5, y-h, 
            just="top", gp=gpar(fontfamily="mono", cex=0.75))
  grid.text(parse(text=expr), .5, y+h, 
            just="bottom", gp=gpar(fontfamily="serif"))
}
drawexpr("z[i] == sqrt(x[i]^2 + y[i]^2)", 1/5)
drawexpr("hat(beta) == (X^t * X)^{-1} * X^t * y", 2/5)
drawexpr("bar(x) == sum(frac(x[i], n), i==1, n)", 3/5)
drawexpr("paste(\"Temperature (\", degree, \"C) in 2003\")", 4/5)
grid.rect(gp=gpar(col="grey"))



}
figure3.23 <- function() {
par(mar=rep(1, 4))
plot(0:1, 0:1, type="n", axes=FALSE, ann=FALSE)
usr <- par("usr")
pin <- par("pin")
xcm <- diff(usr[1:2])/(pin[1]*2.54)
ycm <- diff(usr[3:4])/(pin[2]*2.54)

par(xpd=NA)
rect(0 + 0.2*xcm, 0 - 0.2*ycm,
     1 + 0.2*xcm, 1 - 0.2*ycm,
     col="grey", border=NA)

rect(0, 0, 1, 1, col="white")
segments(seq(1, 8, 0.1)*xcm, 0,
         seq(1, 8, 0.1)*xcm, 
         c(rep(c(0.5, rep(0.25, 4), 
                 0.35, rep(0.25, 4)),
               7), 0.5)*ycm)
text(1:8*xcm, 0.6*ycm, 0:7, adj=c(0.5, 0))
text(8.2*xcm, 0.6*ycm, "cm", adj=c(0, 0))




}
figure3.24 <- function() {
layout(matrix(1:2, ncol=1), heights=1:2/6.5)
par(cex=0.7)
drunkenness <- ts(c(3875, 4846, 5128, 5773, 7327, 
                    6688, 5582, 3473, 3186,
                    rep(NA, 51)),
                  start=1912, end=1971)

# Have to copy-and-paste to shrink the mtext text (arggh!)
par(mar=c(5, 6, 2, 4))
plot(drunkenness, lwd=3, col="grey", ann=FALSE, las=2)
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
               fg="grey", inches=FALSE, add=TRUE)
     })

mtext("symbols(..., add=TRUE)", font=2, side=3, line=1)



}
figure3.25 <- function() {
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
        col="grey", border=NA)

box(col="grey")
plot (yy ~ xx, type="n", axes=FALSE, ann=FALSE)
polygon(c(xx[1], xx, xx[n]), c(min(yy), yy, min(yy)), 
        col="grey", border=NA)

usr <- par("usr")
rect(usr[1], usr[3], usr[2], hline, col="white", border=NA)

box(col="grey")
plot (yy ~ xx, type="n", axes=FALSE, ann=FALSE)
polygon(c(xx[1], xx, xx[n]), c(min(yy), yy, min(yy)), 
        col="grey", border=NA)

usr <- par("usr")
rect(usr[1], usr[3], usr[2], hline, col="white", border=NA)

lines(xx, yy)

box(col="grey")
plot (yy ~ xx, type="n", axes=FALSE, ann=FALSE)
polygon(c(xx[1], xx, xx[n]), c(min(yy), yy, min(yy)), 
        col="grey", border=NA)

usr <- par("usr")
rect(usr[1], usr[3], usr[2], hline, col="white", border=NA)

lines(xx, yy)

abline (h=hline,col="grey")
box()
axis(1)
axis(2) 




}
figure3.26 <- function() {
moonPhase <- function(x, y, phase, size=.07) {
  # phase 1: first quarter
  #       2: full
  #       3: last quarter
  #       4: new
  # size is in inches
  n <- 17
  angle <- seq(0, 2*pi, length=n)
  xx <- x + cos(angle)*xinch(size)
  yy <- y + sin(angle)*yinch(size)
  if (phase == 4)
    fill <- "black"
  else
    fill <- "white"
  polygon(xx, yy, col=fill)
  if (phase == 1)
    polygon(xx[(n/4):(n*3/4) + 1],
            yy[(n/4):(n*3/4) + 1],
            col="black")
  if (phase == 3)
    polygon(xx[c(1:(n/4 + 1), (n*3/4 + 1):n)],
            yy[c(1:(n/4 + 1), (n*3/4 + 1):n)],
            col="black")
}

# Data from Land Information New Zealand
# http://hydro.linz.govt.nz
# + 1 for daylight saving
hours <- c(18, 18, 19, 20, 21, 22, 23,
           0, 1, 2, 3, 4, 5, 5,
           6, 7, 8, 9, 10, 11, 12,
           13, 13, 14, 15, 15, 16) + 1
hours[7] <- 0 # 23 + 1 = 0 on a 24-hour clock
mins <- c(9, 57, 49, 46, 48, 54, 59,
          59, 52, 41, 28, 14, 1, 52,
          36, 43, 41, 39, 38, 35, 26,
          10, 49, 26, 2, 39, 16)
lowTideDate <- ISOdatetime(2005, 2, c(1:6,8:28),
                           hours, mins, 0)
lowTideHour <- ISOdatetime(2005, 2, 1,
                           hours, mins, 0)
phases <- ISOdatetime(2005, 2, c(2, 9, 16, 24),
                      c(19, 10, 12, 16) + 1,
                      c(28, 30, 16, 55), 0)
mainHours <- ISOdatetime(2005, 2, 1,
                         c(0, 4, 8, 12, 16, 20, 23), 
                         c(rep(0, 6), 59), 
                         c(rep(0, 6), 59))

library(pixmap)
# Original image from NASA
# http://grin.hq.nasa.gov/ABSTRACTS/GPN-2000-000473.html
moon <- read.pnm(system.file(file.path("Images", 
                                       "GPN-2000-000473halfsize.pnm"), 
                             package="RGraphics"))
par(pty="s", xaxs="i", yaxs="i", cex=.7)
plot.new()
addlogo(moon, 0:1, 0:1, asp=1)
par(new=TRUE, xaxs="r", yaxs="r", las=1)
plot(lowTideDate, lowTideHour, type="n",
     ylim=range(mainHours), axes=FALSE, ann=FALSE)
# dashed reference lines
midday <- ISOdatetime(2005, 2, 1, 12, 0, 0)
abline(h=midday, v=phases,
       col="white", lty="dashed")
# grey "repeat" of tide info to show gradient
lines(lowTideDate[6:7], 
      c(ISOdatetime(2005, 1, 31,
                    hours[6], mins[6], 0),
        lowTideHour[7]),
      lwd=2, col="grey50")
points(lowTideDate[6:7], 
       c(ISOdatetime(2005, 1, 31,
                     hours[6], mins[6], 0),
         lowTideHour[7]),
       pch=16, cex=.7, col="grey50")
for (subset in list(1:6, 7:27)) {
  lines(lowTideDate[subset], lowTideHour[subset],
        lwd=2, col="white")
  points(lowTideDate[subset], lowTideHour[subset],
         pch=16, cex=.7, col="white")
}
box()
axis.POSIXct(1, lowTideDate)
axis.POSIXct(2, at=mainHours, format="%H:%M")
mtext("Time of Low Tide (NZDT)", side=2, line=4, las=0)
mtext("Auckland, New Zealand 2005", side=1, line=3)
axis(3, at=phases, labels=FALSE)
par(xpd=NA)
ymax <- par("usr")[4]
for (i in 1:4)
  moonPhase(phases[i], ymax + yinch(.2), c(3, 4, 1, 2)[i])
mtext("Phases of the Moon", side=3, line=3)



}
figure3.27 <- function() {
par(mfrow=c(1, 2), mar=c(3, 3, 1, 1), cex=0.7)
y <- sample(1:10)
midpts <- barplot(y, col=" light grey")
width <- diff(midpts[1:2])/4
left <- rep(midpts, y - 1) - width
right <- rep(midpts, y - 1) + width
heights <- unlist(apply(matrix(y, ncol=10), 
                        2, seq))[-cumsum(y)]
segments(left, heights, right, heights,
         col="white")

with(ToothGrowth, 
     {
       boxplot(len ~ supp, border="grey", 
               col="light grey", boxwex=0.5)
       points(jitter(rep(1:2, each=30), 0.5), 
              unlist(split(len, supp)),
              cex=0.5, pch=16)
     })




}
figure3.28 <- function() {
library(maps)
coplot(lat ~ long | depth, data = quakes, number=4, 
       panel=function(x, y, ...) {
         usr <- par("usr")
         rect(usr[1], usr[3], usr[2], usr[4], col="white")
         map("world2", regions=c("New Zealand", "Fiji"),
             add=TRUE, lwd=0.1, fill=TRUE, col="grey")
         text(180, -13, "Fiji", adj=1, cex=0.7)
         text(170, -35, "NZ", cex=0.7)
         points(x, y, pch=".")
       })




}
figure3.29 <- function() {
par(mar=rep(0, 4), lwd=0.1)
z <- 2 * volcano        
x <- 10 * (1:nrow(z))   
y <- 10 * (1:ncol(z))   
trans <- persp(x, y, z, theta = 135, phi = 30, 
               scale = FALSE, ltheta = -120, 
               box = FALSE)
box(col="grey", lwd=1)

trans3d <- function(x,y,z,pmat) {
  tmat <- cbind(x,y,z,1)%*% pmat
  tmat[,1:2] / tmat[,4]
}

summit <- trans3d(x[20], y[31], max(z), trans)
points(summit[1], summit[2], pch=16)
summitlabel <- trans3d(x[20], y[31], max(z) + 50, trans)
text(summitlabel[1], summitlabel[2], "Summit")

drawRoad <- function(x, y, z, trans) {
  road <- trans3d(x, y, z, trans)
  lines(road[,1], road[,2], lwd=5)
  lines(road[,1], road[,2], lwd=3, col="grey")
}
with(volcano.summitRoad,
     drawRoad(srx, sry, srz, trans))
with(volcano.upDownRoad,
     {
       clipudx <- udx
       clipudx[udx < 230 & udy < 300 | 
               udx < 150 & udy > 300] <- NA
       drawRoad(clipudx, udy, udz, trans)
     })
with(volcano.accessRoad,
     drawRoad(arx, ary, arz, trans))




}
figure3.30 <- function() {
groups <- c("cows", "sheep", "horses", 
            "elephants", "giraffes")
males <- sample(1:10, 5)
females <- sample(1:10, 5)

par(mar=c(0.5, 5, 0.5, 1))

plot.new()
plot.window(xlim=c(-10, 10), ylim=c(-1.5, 5.5))

ticks <- seq(-10, 10, 5)
y <- 1:5
h <- 0.2

lines(rep(0, 2), c(-1.5, 5.5), col="grey")
segments(-10, y, 10, y, lty="dotted")
rect(-males, y-h, 0, y+h, col="dark grey")
rect(0, y-h, females, y+h, col="light grey")
mtext(groups, at=y, adj=1, side=2, las=2)
par(cex.axis=0.5, mex=0.5)
axis(1, at=ticks, labels=abs(ticks), pos=0)

tw <- 1.5*strwidth("females")
rect(-tw, -1-h, 0, -1+h, col="dark grey")
rect(0, -1-h, tw, -1+h, col="light grey")
text(0, -1, "males", pos=2)
text(0, -1, "females", pos=4)

box("inner", col="grey")



}
plot.newclass <- 
  function(x, y=NULL, 
           main="", sub="",
           xlim=NULL, ylim=NULL,
           axes=TRUE, ann=par("ann"),
           col=par("col"),
           ...) {
  xy <- xy.coords(x, y)
  if (is.null(xlim))
    xlim <- range(xy$x[is.finite(xy$x)])
  if (is.null(ylim))
    ylim <- range(xy$y[is.finite(xy$y)])
  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))
  plot.new()
  plot.window(xlim, ylim, ...)
  points(xy$x, xy$y, col=col, ...)
  if (axes) {
    axis(1)
    axis(2)
    box()
  }
  if (ann) 
    title(main=main, sub=sub, 
          xlab=xy$xlab, ylab=xy$ylab, ...)
}


figure5.1 <- function() {
pushViewport(viewport(layout=grid.layout(2, 2), gp=gpar(cex=0.6)))
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
grid.text("temperature", y=unit(-3, "lines"))
grid.text("pressure", x=unit(-3, "lines"), rot=90)

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
grid.text("temperature", y=unit(-3, "lines"))
grid.text("pressure", x=unit(-3, "lines"), rot=90)

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
grid.text("temperature", y=unit(-3, "lines"))
grid.text("pressure", x=unit(-3, "lines"), rot=90)

upViewport(2)
grid.rect(gp=gpar(lty="dashed"))
downViewport("plotRegion")
grid.text("Pressure (mm Hg)\nversus\nTemperature (Celsius)",
          x=unit(150, "native"), y=unit(600, "native"))




}
figure5.2 <- function() {
grid.rect(gp=gpar(col="grey"))
grid.circle(x=seq(0.1, 0.9, length=100), 
            y=0.5 + 0.4*sin(seq(0, 2*pi, length=100)),
            r=abs(0.1*cos(seq(0, 2*pi, length=100))))




}
figure5.3 <- function() {
grid.rect(gp=gpar(col="grey"))
angle <- seq(0, 2*pi, length=50)
grid.arrows(x=seq(0.1, 0.5, length=50), 
            y=0.5 + 0.3*sin(angle))
grid.arrows(grob=segmentsGrob(6:8/10, 0.2, 7:9/10, 0.8))




}
figure5.4 <- function() {
grid.rect(gp=gpar(col="grey"))
angle <- seq(0, 2*pi, length=10)[-10]
grid.polygon(x=0.25 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
             gp=gpar(fill="grey"))
grid.polygon(x=0.75 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
             id=rep(1:3, each=3),
             gp=gpar(fill="grey"))




}
figure5.5 <- function() {
grid.rect(gp=gpar(col="grey"))
pushViewport(viewport(gp=gpar(col="grey")))
grid.text("very snug", 0.4, unit(1, "inches"), just=c("left", "bottom"))
grid.arrows(x=0.4, y=unit(0:1, "inches"), ends="both",
  length=unit(1, "mm"))
grid.text("1 inch", unit(0.4, "npc") + unit(0.5, "lines"), 
  unit(0.5, "inches"), rot=90)
grid.arrows(x=c(0, 0.4), y=unit(1, "inches"), ends="both",
  length=unit(1, "mm"))
grid.text(unit(0.4, "npc"), 0.2, unit(1, "inches") + unit(0.5, "lines"))
popViewport()
grid.rect(x=unit(0.4, "npc"), y=unit(1, "inches"),
          width=stringWidth("very snug"), 
          height=unit(1, "lines"),
          just=c("left", "bottom"))




}
figure5.6 <- function() {
grid.rect(gp=gpar(col="grey"))
pushViewport(viewport(gp=gpar(fontsize=10)))
grid.rect(x=0.33, height=0.7, width=0.2, gp=gpar(fill="black"))
grid.rect(x=0.66, height=0.7, width=0.2)
grid.text("grid.rect()", x=0.66, rot=90)
grid.text("grid.rect(gp=gpar(fill=\"black\"))", x=0.33, rot=90, 
  gp=gpar(fontsize=8, col="white"))
popViewport()



}
figure5.7 <- function() {
grid.rect(gp=gpar(col="grey"))
levels <- round(seq(90, 10, length=25))
greys <- paste("grey", c(levels, rev(levels)), sep="")
grid.circle(x=seq(0.1, 0.9, length=100), 
            y=0.5 + 0.4*sin(seq(0, 2*pi, length=100)),
            r=abs(0.1*cos(seq(0, 2*pi, length=100))),
            gp=gpar(col=greys))




}
figure5.8 <- function() {
grid.rect(gp=gpar(col="grey"))
angle <- seq(0, 2*pi, length=11)[-11]
grid.polygon(x=0.25 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
             id=rep(1:2, c(7, 3)),
             gp=gpar(fill=c("grey", "white")))
angle[4] <- NA
grid.polygon(x=0.75 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
             id=rep(1:2, c(7, 3)),
             gp=gpar(fill=c("grey", "white")))

angle <- seq(0, 2*pi, length=11)[4]
grid.text("NA", x=0.75 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle),)



}
figure5.9 <- function() {
vp1 <- 
viewport(x=unit(0.4, "npc"), y=unit(1, "cm"),
         width=stringWidth("very very snug indeed"), 
         height=unit(6, "lines"),
         just=c("left", "bottom"))

grid.show.viewport(scale.col="grey", border.fill="white", vp.col="black", vp.fill="grey", vp1, newpage=FALSE)
pushViewport(viewport(.5, .5, .8, .8))
pushViewport(vp1)
grid.rect()
grid.text("very very snug indeed", 
          gp=gpar(col="white"))
popViewport(2)



}
figure5.10 <- function() {
grid.rect(gp=gpar(col="grey"))
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
figure5.11 <- function() {
grid.rect(gp=gpar(col="grey"))
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
figure5.12 <- function() {
grid.rect(gp=gpar(col="grey"))
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
figure5.13 <- function() {
grid.rect(gp=gpar(col="grey"))
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
figure5.14 <- function() {
grid.rect(gp=gpar(col="grey"))
pushViewport(viewport(w=.5, h=.5, clip="on"))
grid.rect()
grid.circle(r=.7, gp=gpar(lwd=20))

pushViewport(viewport(clip="inherit"))
grid.circle(r=.7, gp=gpar(lwd=10, col="grey"))

pushViewport(viewport(clip="off"))
grid.circle(r=.7)
popViewport(3)




}
figure5.15 <- function() {
grid.rect(gp=gpar(col="grey"))
pushViewport(viewport(gp=gpar(fill="grey", fontsize=10)))
grid.text("viewport(gp=gpar(fill=\"grey\"))", y=0.925)
grid.rect(x=0.33, height=0.7, width=0.2)
grid.text("grid.rect()", x=0.33, rot=90)
grid.rect(x=0.66, height=0.7, width=0.2, gp=gpar(fill="black"))
grid.text("grid.rect(gp=gpar(fill=\"black\"))", x=0.66, rot=90, 
  gp=gpar(fontsize=8, col="white"))
popViewport()



}
figure5.16 <- function() {
labelvp <- function(name, col="grey", tcol="white", clipOff=TRUE) {
  seekViewport(name)
  if (clipOff)
    pushViewport(viewport(clip="off"))
  grid.rect(gp=gpar(col=col, lwd=5))
  grid.rect(x=0, y=1, width=unit(1, "strwidth", name) + unit(2, "mm"),
    height=unit(1, "lines"), just=c("left", "top"),
    gp=gpar(fill=col, col=NULL))
  grid.text(name, x=unit(1, "mm"), y=unit(1, "npc") - unit(1, "mm"),
    just=c("left", "top"), gp=gpar(col=tcol))
  upViewport(0)
}



vplay <- grid.layout(3, 3, 
                     respect=rbind(c(0, 0, 0), 
                                   c(0, 1, 0), 
                                   c(0, 0, 0)))



pushViewport(viewport(width=0.95, height=0.95))
grid.rect(gp=gpar(col="light grey"))
pushViewport(viewport(layout=vplay))

pushViewport(viewport(layout.pos.col=2, name="col2"))
upViewport()
pushViewport(viewport(layout.pos.row=2, name="row2"))

labelvp("col2", "black")
labelvp("row2")



}
figure5.17 <- function() {
unitlay <- 
  grid.layout(3, 3, 
              widths=unit(c(1, 1, 2), 
                          c("inches", "null", "null")), 
              heights=unit(c(3, 1, 1), 
                           c("lines", "null", "null")))



pushViewport(viewport(gp=gpar(cex=0.8)))
grid.show.layout(unitlay, bg="white", 
                 cell.border="black", cell.fill="grey90", 
                 label.col="black", unit.col="black",
                 newpage=FALSE)
popViewport()



}
figure5.18 <- function() {
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



grid.rect(gp=gpar(col="grey"))
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
  gp=gpar(col="grey", lty="dashed"))
grid.segments(0, y, 1, y,
  gp=gpar(col="grey", lty="dashed"))
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
figure5.19 <- function() {

n <- 7
primtest2 <- function(nas, na) {
  angle <- seq(0, 2*pi, length=n+1)[-(n+1)]
  y <- 0.5 + 0.4*sin(angle)
  x <- 0.5 + 0.4*cos(angle)
  if (any(nas))
    grid.text(paste("NA", (1:n)[nas], sep=""),
              x[nas], y[nas], gp=gpar(col="grey"))
  x[nas] <- na
  y[nas] <- na
  grid.polygon(x, y, gp=gpar(fill="light grey", col=NA))
  grid.arrows(grob=linesGrob(x, y),
              gp=gpar(lwd=5))
  grid.move.to(x[1], y[1])
  for (i in 2:n) {
    grid.line.to(x[i], y[i], gp=gpar(col="white"))
  }
}
celltest <- function(r, c, nas, na) {
  pushViewport(viewport(layout.pos.col=c,
                        layout.pos.row=r))
  primtest2(nas, na)
  grid.rect(width=0.9, height=0.9, gp=gpar(col="grey"))
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
figure5.20 <- function() {
depthgroup <- equal.count(quakes$depth, number=3, overlap=0)



trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
trellis.par.set(list(dot.symbol=list(pch=1), axis.text=list(cex=0.7)))
temp <- xyplot(lat ~ long | depthgroup,   
               data=quakes, pch=".",
               layout=c(3, 1))
print(temp, 
      panel.width=list(1.21, "inches"), 
      panel.height=list(1.5, "inches"))




}
figure5.21 <- function() {
depthgroup <- equal.count(quakes$depth, number=3, overlap=0)



trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
print(
xyplot(lat ~ long | depthgroup, data=quakes, pch=".",
       panel=function(...) {
         grid.points(174.75, -36.87, pch=16, 
                     size=unit(2, "mm"),
                     default.units="native")
         grid.text("Auckland",
                   unit(174.75, "native") - unit(2, "mm"),
                   unit(-36.87, "native"),
                   just="right")
         panel.xyplot(...)
       })

)



}
figure5.22 <- function() {
depthgroup <- equal.count(quakes$depth, number=3, overlap=0)



labelvp <- function(name, col="grey", tcol="white", clipOff=TRUE) {
  seekViewport(name)
  if (clipOff)
    pushViewport(viewport(clip="off"))
  grid.rect(gp=gpar(col=col, lwd=5))
  grid.rect(x=0, y=1, width=unit(1, "strwidth", name) + unit(2, "mm"),
    height=unit(1, "lines"), just=c("left", "top"),
    gp=gpar(fill=col, col=NULL))
  grid.text(name, x=unit(1, "mm"), y=unit(1, "npc") - unit(1, "mm"),
    just=c("left", "top"), gp=gpar(col=tcol))
  upViewport(0)
}



lvp <- viewport(x=0, 
                width=unit(1, "npc") - unit(1, "inches"),
                just="left", name="lvp")
tvp <- viewport(x=1, width=unit(1, "inches"), 
                just="right", name="tvp")



lplot <- xyplot(lat ~ long | depthgroup, 
                data=quakes, pch=".",
                layout=c(1, 3), aspect=1, 
                index.cond=list(3:1))



trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
pushViewport(lvp)
print(lplot, newpage=FALSE, prefix="plot1")
upViewport()

labelvp("lvp")
labelvp("plot1.panel.1.3.off.vp")
pushViewport(tvp)
grid.text("Largest\nEarthquake", x=unit(2, "mm"), 
          y=unit(1, "npc") - unit(0.5, "inches"), 
          just="left")

labelvp("tvp")
downViewport("tvp")
grid.move.to(unit(1, "mm"), 
             unit(1, "npc") - unit(0.5, "inches"))
seekViewport("plot1.panel.1.3.off.vp")
grid.arrows(grob=lineToGrob(unit(167.62, "native") +
                            unit(1, "mm"),
                            unit(-15.56, "native")),
            length=unit(3, "mm"), type="closed", 
            angle=10, gp=gpar(fill="black"))
grid.circle(unit(167.62, "native"), 
            unit(-15.56, "native"),
            r=unit(1, "mm"),
            gp=gpar(lwd=0.1))




}
figure1.2 <- function() {


#
#  Comment:
# 
#  Examples of the use of standard high-level plotting functions.
# 
#  In each case, extra output is also added using low-level 
#  plotting functions.
#


par(mfrow=c(3, 2))

# Scatterplot
x <- c(0.5, 2, 4, 8, 12, 16)
y1 <- c(1, 1.3, 1.9, 3.4, 3.9, 4.8)
y2 <- c(4, .8, .5, .45, .4, .3)
par(las=1, mar=c(4, 4, 2, 4))
plot.new()
plot.window(range(x), c(0, 6))
lines(x, y1)
lines(x, y2)
points(x, y1, pch=16, cex=2)
points(x, y2, pch=21, bg="white", cex=2)
par(col="grey50", fg="grey50", col.axis="grey50")
axis(1, at=seq(0, 16, 4))
axis(2, at=seq(0, 6, 2))
axis(4, at=seq(0, 6, 2))
box(bty="u")
mtext("Travel Time (s)", side=1, line=2, cex=0.8)
mtext("Responses per Travel", side=2, line=2, las=0, cex=0.8)
mtext("Responses per Second", side=4, line=2, las=0, cex=0.8)
text(4, 5, "Bird 131")
par(mar=c(5.1, 4.1, 4.1, 2.1), col="black", fg="black", col.axis="black")

# Histogram
# Random data
Y <- rnorm(50)
# Make sure no Y exceed [-3.5, 3.5]
Y[Y < -3.5 | Y > 3.5] <- NA
x <- seq(-3.5, 3.5, .1)
dn <- dnorm(x)
par(mar=c(4.5, 4.1, 3.1, 0))
hist(Y, breaks=seq(-3.5, 3.5), ylim=c(0, 0.5), 
     col="grey80", freq=FALSE)
lines(x, dnorm(x), lwd=2)
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Barplot
# Modified from example(barplot)
par(mar=c(2, 3.1, 2, 2.1))
midpts <- barplot(VADeaths, col=grey(0.5 + 1:5/12), 
                  names=rep("", 4))
mtext(sub(" ", "\n", colnames(VADeaths)),
      at=midpts, side=1, line=0.5, cex=0.5)
text(rep(midpts, each=5), apply(VADeaths, 2, cumsum) - VADeaths/2,
     VADeaths, col=rep(c("white", "black"), times=2:3, cex=0.8))
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Boxplot
# Modified example(boxplot) - itself from suggestion by Roger Bivand
par(mar=c(3, 4.1, 2, 0))
     boxplot(len ~ dose, data = ToothGrowth,
             boxwex = 0.25, at = 1:3 - 0.2,
             subset= supp == "VC", col="grey90",
             xlab="",
             ylab="tooth length", ylim=c(0,35))
     mtext("Vitamin C dose (mg)", side=1, line=2.5, cex=0.8)
     boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
             boxwex = 0.25, at = 1:3 + 0.2,
             subset= supp == "OJ", col="grey70")
     legend(1.5, 9, c("Ascorbic acid", "Orange juice"), bty="n",
            fill = c("grey90", "grey70"))
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Persp
# Almost exactly example(persp)
    x <- seq(-10, 10, length= 30)
     y <- x
     f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
     z <- outer(x, y, f)
     z[is.na(z)] <- 1
# 0.5 to include z axis label
par(mar=c(0, 0.5, 0, 0), lwd=0.1)
     persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "grey80")
par(mar=c(5.1, 4.1, 4.1, 2.1), lwd=1)

# Piechart
# Example 4 from help(pie)
par(mar=c(0, 2, 1, 2), xpd=FALSE, cex=0.5)
     pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
     names(pie.sales) <- c("Blueberry", "Cherry",
         "Apple", "Boston Cream", "Other", "Vanilla")
     pie(pie.sales, col = gray(seq(0.4,1.0,length=6)))




}
figure1.3 <- function() {

#
# Comment:
#
# A sophisticated example of adding further output to a basic plot.
# 
# Most of the functions defined are just for calculating values
# relevant to the data analysis.  
# 
# The function plotPars() is the one of interest for seeing how
# the drawing of the plot is done.
#


params <- function(N, breaks, p=seq(0.001, 1, length=100)) {
  list(N=N, T=1/breaks, p=p, q=1-p)
}

pdfcomp <- function(comp, params) {
  n <- params$T
  p <- params$p
  q <- params$q
  y <- round(comp/n)
  choose(n, comp)*p^comp*q^(n-comp) / (1 - q^n)
}

# Expected num sherds (for a vessel) [=completeness]
expcomp <- function(params) {
  params$T*params$p/(1-params$q^params$T)
}

# Variance of num sherds (for a vessel)
varcomp <- function(params) {
  n <- params$T
  p <- params$p
  q <- params$q
  # From Johnson & Kotz
  (n*p*q / (1 - q^n)) - (n^2*p^2*q^n / (1 - q^n)^2)
  # n^2 times Thomas Yee's formula
  # n^2*((p*(1 + p*(n - 1)) / (n*(1 - q^n))) - (p^2 / (1 - q^n)^2))
}

# Expected value of completeness (for a sample of vessels)
expmeancomp <- function(params) {
  expcomp(params)
}

# Variance of completeness (for a sample of vessels)
# Use the expected number of vessels in sample as denominator
varmeancomp <- function(params) {
  varcomp(params)/(numvess(params))
}

numvess <- function(params) {
  params$N*(1-params$q^params$T)
}

ecomp <- function(p, T, comp) {
  q <- 1 - p
  T*p/(1 - q^T) - comp
}

estN <- function(comp, broke, n) {
  T <- 1/broke
  n / (1 - (1 - uniroot(ecomp, c(0.00001, 1), T=T, comp=comp)$root)^T)
}

nvessscale <- function(params, xlim, ylim, new=TRUE) {
  if (new)
    par(new=TRUE)
  plot(0:1, c(1, params$N), type="n", axes=!new, ann=FALSE,
       xlim=xlim, ylim=ylim)
}

compscale <- function(params, xlim, ylim, new=TRUE) {
  if (new)
    par(new=TRUE)
  plot(0:1, c(1, params$T), type="n", axes=!new, ann=FALSE,
       xlim=xlim, ylim=ylim)
}

lowerCI <- function(p, N, breaks, lb) {
  params <- params(N, breaks, p)
  expmeancomp(params) - 2*sqrt(varmeancomp(params)) - lb
}

upperCI <- function(p, N, breaks, lb) {
  params <- params(N, breaks, p)
  expmeancomp(params) + 2*sqrt(varmeancomp(params)) - lb
}

critP <- function(comp, params) {
  c(uniroot(lowerCI, c(0.00001, 1), N=params$N,
            breaks=1/params$T, lb=max(comp))$root,
    if (upperCI(0.00001, params$N, 1/params$T, min(comp)) > 0) 0
    else uniroot(upperCI, c(0.00001, 1), N=params$N,
                 breaks=1/params$T, lb=min(comp))$root)
}

anncomp <- function(params, comp, xlim, ylim, cylim) {
  cp <- critP(comp, params)
  nv <- numvess(params(params$N, 1/params$T, cp))
  nvessscale(params, xlim, ylim)
  polygon(c(cp[2], cp[2], 0, 0, cp[1], cp[1]),
          c(0, nv[2], nv[2], nv[1], nv[1], 0),
          col="grey90", border=NA)
  text(0, nv[1], paste(round(nv[1]),
                       " (", round(100*nv[1]/params$N), "%)", sep=""),
       adj=c(0, 0), col="grey")
  text(0, nv[2], paste(round(nv[2]), 
                       " (", round(100*nv[2]/params$N), "%)", sep=""),
       adj=c(0, 1), col="grey")
  compscale(params, xlim, cylim)
  segments(1, min(comp), cp[2], comp, col="grey")
  segments(1, max(comp), cp[1], comp, col="grey")
  text(1, comp, paste(comp, collapse="-"), adj=c(1, 0), col="grey")
}

plotPars <- function(params, comp, xlim=NULL, ylim=NULL) {
  mean <- expmeancomp(params)
  var <- 2*sqrt(varmeancomp(params))
  lb <- mean - var
  ub <- mean + var
  par(mar=c(5, 4, 4, 4))
  if (is.null(ylim))
    cylim <- ylim
  else
    cylim <- c(1 + ((ylim[1] - 1)/(params$N - 1))*(params$T - 1),
               1 + ((ylim[2] - 1)/(params$N - 1))*(params$T - 1))
  nvessscale(params, xlim, ylim, new=FALSE)
  compscale(params, xlim, cylim)
  polygon(c(params$p, rev(params$p)), c(lb, rev(ub)),
          col="grey90", border=NA)
  anncomp(params, comp, xlim, ylim, cylim)
  nvessscale(params, xlim, ylim)
  mtext("Number of Vessels", side=2, line=3)
  mtext("Sampling Fraction", side=1, line=3)
  lines(params$p, numvess(params))
  par(new=TRUE)
  compscale(params, xlim, cylim)
  mtext("Completeness", side=4, line=3)
  axis(4)
  lines(params$p, mean, lty="dashed")
  lines(params$p, lb, lty="dotted")
  lines(params$p, ub, lty="dotted")
  mtext(paste("N = ", round(params$N),
              "     brokenness = ", round(1/params$T, 3), sep=""),
        side=3, line=2)
}

par(cex=0.8, mar=c(3, 3, 3, 3))
p6 <- params(estN(1.2, 0.5, 200), 0.5)
plotPars(p6, 1.2)
nvessscale(p6, NULL, NULL)
pcrit <- 1 - (1 - 200/estN(1.2, 0.5, 200))^(1/p6$T)
lines(c(0, pcrit), c(200, 200))
lines(c(pcrit, pcrit), c(200, 0))



}
figure1.4 <- function() {

#
# Comment:
#
# A slightly modified version of Figure 1.1 from 
# Cleveland's book "Visualizing Data"
#


library(lattice)
trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
trellis.par.set(list(fontsize=list(text=6),
	             par.xlab.text=list(cex=1.5),
                     add.text=list(cex=1.5),
                     superpose.symbol=list(cex=.5)))
key <- simpleKey(levels(barley$year), space = "right")
key$text$cex <- 1.5
print(
     dotplot(variety ~ yield | site, data = barley, groups = year,
             key = key,
             xlab = "Barley Yield (bushels/acre) ",
             aspect=0.5, layout = c(1,6), ylab=NULL)
)



}
figure1.5 <- function() {

#
# Comment:
#
# A bit of mucking around is required to get the second (whole-world)
# map positioned correctly;  this provides an example of calling a 
# plotting function to perform calculations but do no drawing (see the
# second call to the map() function).
#
# Makes use of the "maps" and "mapproj" packages to draw the maps.
#


library(maps)
par(mar=rep(0, 4))
map("nz", fill=TRUE, col="grey80")
points(174.75, -36.87, pch=16, cex=2)
arrows(172, -36.87, 174, -36.87, lwd=3)
text(172, -36.87, "Auckland  ", adj=1, cex=2)
# mini world map as guide
maplocs <- map(projection="sp_mercator", wrap=TRUE, lwd=0.1, 
               col="grey", ylim=c(-60, 75),
               interior=FALSE, orientation=c(90, 180, 0), add=TRUE,
               plot=FALSE)
xrange <- range(maplocs$x, na.rm=TRUE)
yrange <- range(maplocs$y, na.rm=TRUE)
aspect <- abs(diff(yrange))/abs(diff(xrange))
# customised to 6.5 by 4.5 figure size
par(fig=c(0.99 - 0.5, 0.99, 0.01, 0.01 + 0.5*aspect*4.5/6.5), 
    mar=rep(0, 4), new=TRUE)
plot.new()
plot.window(xlim=xrange,
            ylim=yrange)
map(projection="sp_mercator", wrap=TRUE, lwd=0.1, ylim=c(-60, 75),
    interior=FALSE, orientation=c(90, 180, 0), add=TRUE)
symbols(-.13, -0.8, circles=1, inches=0.1, add=TRUE)



}
figure1.6 <- function() {
"polar.plot" <- 
function (r, theta, theta.zero = 0, theta.clw = FALSE, method = 1, 
    rlabel.axis = 0, dir = 8, rlimits = NULL, grid.circle.pos = NULL, 
    grid.lwd = 1, grid.col = "black", points.pch = 20, points.cex = 1, 
    lp.col = "black", lines.lwd = 1, lines.lty = 1, polygon.col = NA, 
    polygon.bottom = TRUE, overlay = NULL, pi2.lab = TRUE, text.lab = NULL, 
    num.lab = NULL, rlabel.method = 1, rlabel.pos = 3, rlabel.cex = 1, 
    rlabel.col = "black", tlabel.offset = 0.1, tlabel.cex = 1.5, 
    tlabel.col = "black", main = NULL, sub = NULL) 
{ 
# r: (vector of) radial data. 
# theta: (vector of) angular data (in radians). 
# theta.zero: angular direction on plot of theta = 0 (in radians). 
# theta.clw: clockwise orientation of theta values (default = FALSE). 
# 
# method: (plotting of (r,theta)-data): 
# 1: points (default) 
# 2: line 
# 3: polygon 
# 
# rlabel.axis: angular direction on the plot of radial label axis (in radians). 
# dir: number of radial grid lines (default=8). 
# rlimts: Interval for radial axis as a numeric vector: c(lower,upper). Interval will be extended by the default use of pretty()-function. (default = NULL). 
# grid.circle.pos: radial axis position of grid circles as numeric vector of minimum length 2. Overrides the default positioning of grid circles by pretty()-function. (default = NULL). 
# grid.lwd. grid line width. 
# grid.col: grid line color. 
# 
# points.pch: points plotting symbol. 
# point.cex: character expansion factor for points. 
# lp.col: color of points (method 1) or lines (method 2 and method 3). In method 3, set lp.col=0 for polygons without border. 
# lines.lwd: line width for plotting methods 2 and 3 (default = 1). 
# lines.lty: line type (default = 1). 
# polygon.col: color of polygon (defalut = NA). 
# polygon.bottom: polygon to the back i.e. behind the grid (default = TRUE). 
# 
# overlay: NULL (default), no overlay 
# 1, overlay data on existing plot 
# 2, overlay data, grid and labels on existing plot. 
# 
# pi2.lab: angular labels in radians (0, pi/2, pi, 3*pi/2) (default). 
# text.lab: angular axis labels from a character vector c("N","E","S","W") (default = NULL). 
# num.lab: numeric angular axis labels in interval [0;num.lab[ (default = NULL). Number of labels: dir. 
# 
# rlabel.method (plotting of radial axis labels): 
# 0: no radial labels. 
# 1: labels at pretty radial distances (default). 
# 2: exclude label at radial distace 0. 
# 3: exclude label at maximum radial distance. 
# 4: exclude radial labels at distance 0 and at maximum radial distance. 
# rlabel.pos: text position of radial axis labels (NULL,1,2,3,4). 
# rlabel.cex: cex for radial axis labels. 
# rlabel.col: color of the radial labels. 
# 
# tlabel.offset: radial offset for angular axis labels in fraction of maximum radial value (default = 0.1). 
# tlabel.cex: cex for angular axis labels. 
# tlabel.col: angular labels color. 
# 
# main: plot main title. 
# sub: plot sub title. 
   
    fit.rad <- function(x, twop = 2 * pi) { 
        for (i in 1:length(x)) { 
            while (x[i] < 0) x[i] <- x[i] + twop 
            while (x[i] >= twop) x[i] <- x[i] - twop 
        } 
        return(x) 
    } 
    if (is.null(rlimits)) 
        rpretty <- pretty(range(abs(r), 0, na.rm = TRUE)) 
    if (is.numeric(rlimits) & length(rlimits) == 2) 
        rpretty <- pretty(range(abs(rlimits[1]), abs(rlimits[2]))) 
    if (is.numeric(grid.circle.pos) & length(grid.circle.pos) > 
        1) 
        rpretty <- grid.circle.pos 
    lab.dist <- max(rpretty) 
    if (!is.null(text.lab) || is.numeric(num.lab) || pi2.lab) { 
        lab.dist <- lab.dist * (tlabel.offset + 1) 
    } 
    if (is.null(overlay)) { 
        plot.new() 
        ps <- max(lab.dist, max(rpretty)) 
        plot.window(xlim = c(-ps, ps), ylim = c(-ps, ps), asp = 1) 
        title(main = main, sub = sub) 
    } 
    drawgrid <- function() { 
        if (dir > 0) { 
            rDir <- seq(0, 2 * pi, length = dir + 1)[-(dir + 
                1)] 
            segments(0, 0, max(rpretty) * cos(rDir), max(rpretty) * 
                sin(rDir), col = grid.col, lwd = grid.lwd) 
        } 
        grid <- seq(0, 2 * pi, length = 360/4 + 1) 
        for (rad in rpretty) { 
            if (rad > 0) 
                lines(rad * cos(grid), rad * sin(grid), col = grid.col, 
                  lwd = grid.lwd) 
        } 
        if (rlabel.method != 0) { 
            if (rlabel.method == 1) 
                radLabels <- 1:length(rpretty) 
            if (rlabel.method == 2) 
                radLabels <- 2:length(rpretty) 
            if (rlabel.method == 3) 
                radLabels <- 1:(length(rpretty) - 1) 
            if (rlabel.method == 4) { 
                if (length(rpretty) > 2) 
                  radLabels <- 2:(length(rpretty) - 1) 
                else radLabels <- NULL 
            } 
            if (!is.null(radLabels)) { 
                xpos <- rpretty[radLabels] * cos(rlabel.axis) 
                ypos <- rpretty[radLabels] * sin(rlabel.axis) 
                text(xpos, ypos, rpretty[radLabels], cex = rlabel.cex, 
                  pos = rlabel.pos, col = rlabel.col) 
            } 
        } 
        if (!is.numeric(num.lab)) {
            t.lab <- ""
            if (pi2.lab & !is.character(text.lab)) 
                t.lab <- expression(0, pi/2, pi, 3 * pi/2) 
            if (!pi2.lab & is.character(text.lab)) 
                t.lab <- text.lab 
            labDir <- seq(0, 2 * pi, length = length(t.lab) + 
                1)[-(length(t.lab) + 1)] 
            labDir <- fit.rad(theta.zero + (!theta.clw) * labDir - 
                (theta.clw) * labDir) 
            text(lab.dist * cos(labDir), lab.dist * sin(labDir), 
                t.lab, cex = tlabel.cex, col = tlabel.col) 
        } 
        if (!pi2.lab & is.null(text.lab) & is.numeric(num.lab)) { 
            labDir <- seq(0, 2 * pi, length = num.lab + 1)[-(num.lab + 
                1)] 
            labDir <- fit.rad(theta.zero + (!theta.clw) * labDir - 
                (theta.clw) * labDir) 
            text(lab.dist * cos(labDir), lab.dist * sin(labDir), 
                paste(num.lab * labDir/(2 * pi)), cex = tlabel.cex, 
                col = tlabel.col) 
        } 
        if ((is.character(text.lab) & is.numeric(num.lab)) || 
            (is.character(text.lab) & pi2.lab) || (pi2.lab & 
            is.numeric(num.lab))) 
            print("More than one type of angular labels was requested.") 
    } 
    theta2 <- fit.rad(theta.zero + (!theta.clw) * theta - (theta.clw) * 
        theta) 
    cartesian.rt <- cbind(r * cos(theta2), r * sin(theta2)) 
    if (method == 1) { 
        if (is.null(overlay) || overlay == 2) 
            drawgrid() 
        points(cartesian.rt[, 1], cartesian.rt[, 2], col = lp.col, 
            pch = points.pch, cex = points.cex) 
    } 
    if (method == 2) { 
        if (is.null(overlay) || overlay == 2) 
            drawgrid() 
        lines(cartesian.rt[, 1], cartesian.rt[, 2], lwd = lines.lwd, 
            col = lp.col, lty = lines.lty) 
    } 
    if ((method == 2 || method == 3) & length(r) <= 1) 
        print("More than one data point is needed for line and polygon methods.") 
    if (method == 3) { 
        if (!polygon.bottom & (is.null(overlay) || overlay == 
            2)) 
            drawgrid() 
        polygon(cartesian.rt, lwd = lines.lwd, col = polygon.col, 
            border = lp.col, lty = lines.lty) 
        if (polygon.bottom & (is.null(overlay) || overlay == 
            2)) 
            drawgrid() 
    } 
} 




#
# Comment:
#
# Examples using high-level plotting functions from the packages 
# "graphics", "CircStats", and "vcd", plus code contributed in an
# email on the R-help mailing list by Kartsen Bjerre (above).
#
# In each case, additional output has been used to slightly
# customize the output from the plotting function and/or 
# the arguments of the plotting function have been used to
# achieve a non-default appearance for the plot.
#


par(mfrow=c(2, 2), xpd=NA)
# Example 3 from stars
data(mtcars)
     stars(mtcars[, 1:7], locations = c(0,0), radius = FALSE,
           key.loc=c(0,0), main="Motor Trend Cars",
           col.stars=grey(1:32/33))
points(0, 0, pch=16)
# karstenbjerre polar plot
par(mar=rep(1, 4))
div<-50
theta <- seq(0, 2 * pi, length = div + 1)[-(div+1)] 
r<-1:(div) 
textlabels<-c('N','E','S','W')
par(lwd=.1)
polar.plot(r, theta, theta.clw = TRUE, theta.zero = pi/2, 
           text.lab = textlabels , pi2.lab = FALSE, lines.lwd = 3, 
           grid.lwd = 1, grid.col = "grey", rlabel.method = 2, 
           rlabel.axis = pi/2, rlabel.pos = NULL,  
           points.pch = 21, tlabel.offset = 0.3) 
par(lwd=1)
# example 2 from rose.diag from CircStats package
library(CircStats)
data <- runif(50, 0, 2*pi)
par(cex=0.5, mar=rep(1, 4))
rose.diag(data, bins = 18, pts=TRUE, prop=2, shrink=1.2)
par(cex=1)

# Ternary plot
# Based on example 1 of help(ternaryplot)
library(vcd)
par(cex=0.5)
data(Arthritis)

# Build table by crossing Treatment and Sex
tab <- as.table(xtabs(~ I(Sex:Treatment) + Improved, data = Arthritis))

# Mark groups
col <- c("black", "black", "black", "black")
pch <- c(1, 19, 0, 15)

# plot
ternaryplot(tab,
            col = col,
            border="grey",
            pch = pch,
            prop.size = TRUE,
            bg = "white",
            grid="solid",
            grid.color = "grey",
            labels.color = "grey",
            main = "")

detach("package:vcd")



}
figure1.7 <- function() {
library(party)

# CLASSIFICATION
# fitting
library(ipred)
data(GlaucomaM)
glau <- GlaucomaM
levels(glau$Class) <- c("glau", "norm")
fm.class <- ctree(Class ~ ., data = glau)

# visualization
pushViewport(viewport(gp=gpar(cex=0.6)))
plot(fm.class, new=FALSE, terminal.panel=myNode)
popViewport()



}
figure1.8 <- function() {

#
# Comment:
#
# Some simple ideas as a basis for meta-analysis plots.
# 
# The code is modular so that something similar could be achieved
# with different data quite simply.  The actual drawing for these data
# only occurs in the last 10 or so lines of code.
#


# The horizontal gap between columns with content
colgap <- unit(3, "mm")

# The data for column 1
# 
# Of course, many other possible ways to represent the data
# One advantage with this way is that col1$labels can be used
# directly in the calculation of the column widths for the
# main table (see below)
#
# NOTE:  textGrobs are used here so that the fontface (bold in
# some cases) is associated with the label.  In this way, the
# calculation of column widths takes into account the font face.
col1 <- list(labels=
             list(textGrob("Centre", x=0, just="left",
                           gp=gpar(fontface="bold")),
                  textGrob("Thailand", x=0, just="left"),
                  textGrob("Philippines", x=0, just="left"),
                  textGrob("All in situ", x=0, just="left",
                           gp=gpar(fontface="bold")),
                  textGrob("Colombia", x=0, just="left"),
                  textGrob("Spain", x=0, just="left"),
                  textGrob("All invasive", x=0, just="left",
                           gp=gpar(fontface="bold")),
                  textGrob("All", x=0, just="left",
                           gp=gpar(fontface="bold"))),
             rows=c(1, 5, 6, 8, 11, 12, 14, 16))

# Labels in col 1 which are not used to calculate the
# column width (they spill over into col 2)
col1plus <- list(labels=
                 list(textGrob("Carcinoma in situ", x=0, just="left",
                               gp=gpar(fontface="bold")),
                      textGrob("Invasive cancer", x=0, just="left",
                               gp=gpar(fontface="bold"))),
                 rows=c(4, 10))

# Data for column 2
col2 <- list(labels=
             list(textGrob("cases", x=1, just="right",
                           gp=gpar(fontface="bold")),
                  textGrob("327", x=1, just="right"),
                  textGrob("319", x=1, just="right"),
                  textGrob("1462", x=1, just="right",
                           gp=gpar(fontface="bold")),
                  textGrob("96", x=1, just="right"),
                  textGrob("115", x=1, just="right"),
                  textGrob("211", x=1, just="right",
                           gp=gpar(fontface="bold")),
                  textGrob("1673", x=1, just="right",
                           gp=gpar(fontface="bold"))),
             rows=c(1, 5, 6, 8, 11, 12, 14, 16))

# Data for column 3 (width specified as a physical size below)
col3 <- list(OR=c(0.72, 1.27, 1.17, 2.97, 1.86, 2.01, 1.20),
             LL=c(0.52, 0.87, 1.03, 1.42, 0.46, 1.09, 1.07),
             UL=c(1.00, 1.85, 1.32, 6.21, 7.51, 3.71, 1.35),
             rows=c(5, 6, 8, 11, 12, 14, 16),
             # "s" means summary, "n" means normal
             type=c("n", "n", "s", "n", "n", "s", "s"))

# Sizes of boxes
information <- sqrt(1 / ((log(col3$UL) - log(col3$OR))/1.96))
col3$sizes <- information/max(information)

# Width of column 3
col3width <- unit(1.5, "inches")

# Range on the x-axis for column 3
col3$range <- c(0, 4)

# Function to draw a cell in a text column
drawLabelCol <- function(col, j) {
  for (i in 1:length(col$rows)) {
    pushViewport(viewport(layout.pos.row=col$rows[i], layout.pos.col=j))
    # Labels are grobs containing their location so just
    # have to grid.draw() them
    grid.draw(col$labels[[i]])
    popViewport()
  }
}

# Function to draw a non-summary rect-plus-CI
drawNormalCI <- function(LL, OR, UL, size) {
  # NOTE the use of "native" units to position relative to
  # the x-axis scale, and "snpc" units to size relative to
  # the height of the row
  # ("snpc" stands for "square normalised parent coordinates"
  #  which means that the value is calculated as a proportion
  #  of the width and height of the current viewport and the
  #  physically smaller of these is used)
  grid.rect(x=unit(OR, "native"),
            width=unit(size, "snpc"), height=unit(size, "snpc"),
            gp=gpar(fill="black"))
  # Draw arrow if exceed col range
  # convertX() used to convert between coordinate systems
  if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) > 1)
    grid.arrows(x=unit(c(LL, 1), c("native", "npc")),
                length=unit(0.05, "inches"))
  else {
    # Draw line white if totally inside rect
    lineCol <- if ((convertX(unit(OR, "native") + unit(0.5*size, "lines"),
                             "native", valueOnly=TRUE) > UL) &&
                   (convertX(unit(OR, "native") - unit(0.5*size, "lines"),
                             "native", valueOnly=TRUE) < LL))
      "white"
    else
      "black"
    grid.lines(x=unit(c(LL, UL), "native"), y=0.5,
               gp=gpar(col=lineCol))
  }
}

# Function to draw a summary "diamond"
drawSummaryCI <- function(LL, OR, UL, size) {
  # Not sure how to calc the heights of the diamonds so
  # I'm just using half the height of the equivalent rect
  grid.polygon(x=unit(c(LL, OR, UL, OR), "native"),
               y=unit(0.5 + c(0, 0.25*size, 0, -0.25*size), "npc"))
}

# Function to draw a "data" column
drawDataCol <- function(col, j) {
  pushViewport(viewport(layout.pos.col=j, xscale=col$range))
  grid.lines(x=unit(1, "native"), y=0:1)
  # Assume that last value in col is "All"
  grid.lines(x=unit(col$OR[length(col$OR)], "native"),
             y=0:1, gp=gpar(lty="dashed"))
  grid.xaxis(gp=gpar(cex=0.6))
  grid.text("OR", y=unit(-2, "lines"))
  popViewport()
  for (i in 1:length(col$rows)) {
    pushViewport(viewport(layout.pos.row=col$rows[i], layout.pos.col=j,
                          xscale=col$range))
    if (col$type[i] == "n")
      drawNormalCI(col$LL[i], col$OR[i], col$UL[i], col$sizes[i])
    else
      drawSummaryCI(col$LL[i], col$OR[i], col$UL[i], col$sizes[i])
    popViewport()
  }
}

# Draw the table
#
# The table is just a big layout
#
# All rows are the height of 1 line of text
# 
# Widths of column 1 and 2 are based on widths of labels in
# col$labels and col2$labels 
pushViewport(viewport(layout=grid.layout(16, 5,
                        widths=
                        unit.c(max(unit(rep(1, 8), "grobwidth", col1$labels)),
                               colgap,
                               max(unit(rep(1, 8), "grobwidth", col2$labels)),
                               colgap,
                               col3width),
                        heights=unit(rep(1, 16), "lines"))))
drawLabelCol(col1, 1)
drawLabelCol(col1plus, 1)
drawLabelCol(col2, 3)
drawDataCol(col3, 5)
popViewport()
                          



}
figure1.9 <- function() {

#
# Comment:
#
# Code by Arden Miller (Department of Statistics, The University of Auckland).
# 
# Lots of coordinate transformations being done "by hand".
# This code is not really reusable;  just a demonstration that very 
# pretty results are possible if you're sufficiently keen.
#


par(mfrow=c(2, 1), pty="s", mar=rep(1, 4)) 
# Create plotting region and plot outer circle
plot(c(-1.1, 1.2), c(-1.1, 1.2),
     type="n", xlab="", ylab="", 
     xaxt="n", yaxt="n", cex.lab=2.5)
angs <- seq(0, 2*pi, length=500)
XX <- sin(angs)
YY <- cos(angs)
lines(XX, YY, type="l")

# Set constants
phi1 <- pi*2/9
k1 <- sin(phi1)
k2 <- cos(phi1)

# Create grey regions
obsphi <- pi/12
lambdas <- seq(-pi, pi, length=500)
xx <- cos(pi/2 - obsphi)*sin(lambdas)
yy <- k2*sin(pi/2 - obsphi)-k1 * cos(pi/2 - obsphi)*cos(lambdas)
polygon(xx, yy, col="grey")
lines(xx, yy, lwd=2)
theta1sA <- seq(-obsphi, obsphi, length=500)
theta2sA <- acos(cos(obsphi)/cos(theta1sA))
theta1sB <- seq(obsphi, -obsphi, length=500)
theta2sB <-  -acos(cos(obsphi)/cos(theta1sB))
theta1s <- c(theta1sA, theta1sB)
theta2s <- c(theta2sA, theta2sB)
xx <- cos(theta1s)*sin(theta2s+pi/4)
yy <- k2*sin(theta1s)-k1*cos(theta1s)*cos(theta2s+pi/4)
polygon(xx, yy, col="grey")
lines(xx, yy, lwd=2)
xx <- cos(theta1s)*sin(theta2s-pi/4)
yy <- k2*sin(theta1s)-k1*cos(theta1s)*cos(theta2s-pi/4)
polygon(xx, yy, col="grey")
lines(xx, yy, lwd=2)

# Plot longitudes
vals <- seq(0, 7, 1)*pi/8
for(lambda in vals){
sl <- sin(lambda)
cl <- cos(lambda)
phi <- atan(((0-1)*k2*cl)/(k1))
angs <- seq(phi, pi+phi, length=500)
xx <- cos(angs)*sl
yy <- k2*sin(angs)-k1*cos(angs)*cl
lines(xx, yy, lwd=.5)
}

# Grey out polar cap
phi <- 5.6*pi/12
lambdas <- seq(-pi, pi, length=500)
xx <- cos(phi)*sin(lambdas)
yy <- k2*sin(phi)-k1 * cos(phi)*cos(lambdas)
polygon(xx, yy, col="grey")

# Plot Latitudes
vals2 <- seq(-2.8, 5.6, 1.4)*pi/12
for(phi in vals2){
  if (k1*sin(phi) > k2 * cos(phi)) 
    crit <- pi 
  else 
    crit <- acos((-k1*sin(phi))/(k2*cos(phi)))
  lambdas <- seq(-crit, crit, length=500)
  xx <- cos(phi)*sin(lambdas)
  yy <- k2*sin(phi)-k1 * cos(phi)*cos(lambdas)
  lines(xx, yy, lwd=.5)
}


# Plots axes and label
lines(c(0.00, 0.00), c(k2*sin(pi/2), 1.11), lwd=4)
lines(c(0.00, 0.00), c(-1, -1.12), lwd=4)
a2x <- sin(-pi/4)
a2y <- cos(-pi/4)*(-k1)
lines(c(a2x, 1.5*a2x), c(a2y, 1.5*a2y), lwd=4)
k <- sqrt(a2x^2+a2y^2)
lines(c(-a2x/k, 1.2*(-a2x/k)), c(-a2y/k, 1.2*(-a2y/k)), lwd=4)
a3x <- sin(pi/4)
a3y <- cos(pi/4)*(-k1)
lines(c(a3x, 1.5*a3x), c(a3y, 1.5*a3y), lwd=4)
k <- sqrt(a3x^2+a3y^2)
lines(c(-a3x/k, 1.2*(-a3x/k)), c(-a3y/k, 1.2*(-a3y/k)), lwd=4)
text(0.1, 1.12, expression(bold(X[1])))
text(-1.07, -.85, expression(bold(X[2])))
text(1.11, -.85, expression(bold(X[3])))

# set plot region and draw outer circle
plot(c(-1.1, 1.2),  c(-1.1, 1.2),
     type="n", xlab="", ylab="", 
     xaxt="n", yaxt="n", cex.lab=2.5)
angs <- seq(0, 2*pi, length=500)
XX <- sin(angs)
YY <- cos(angs)
lines(XX, YY, type="l")

# set constants
phi1 <- pi*2/9
k1 <- sin(phi1)
k2 <- cos(phi1)
obsphi <- pi/24

# create X2X3 grey region and plot boundary
crit <- acos((-k1*sin(obsphi))/(k2 * cos(obsphi)))
lambdas <- seq(-crit, crit, length=500)
xx1 <- cos(obsphi)*sin(lambdas)
yy1 <- k2*sin(obsphi)-k1 * cos(obsphi)*cos(lambdas)
obsphi <-  -pi/24
crit <- acos((-k1*sin(obsphi))/(k2 * cos(obsphi)))
lambdas <- seq(crit, -crit, length=500)
xx3 <- cos(obsphi)*sin(lambdas)
yy3 <- k2*sin(obsphi)-k1 * cos(obsphi)*cos(lambdas)
ang1 <-  atan(xx1[500]/yy1[500])
ang2 <- pi+atan(xx3[1]/yy3[1])
angs <- seq(ang1, ang2, length=50)
xx2 <- sin(angs)
yy2 <- cos(angs)
ang4 <-  atan(xx1[1]/yy1[1])
ang3 <-  -pi+ atan(xx3[500]/yy3[500])
angs <- seq(ang3, ang4, length=50)
xx4 <- sin(angs)
yy4 <- cos(angs)
xxA <- c(xx1, xx2, xx3, xx4)
yyA <- c(yy1, yy2, yy3, yy4)
polygon(xxA, yyA, border="grey", col="grey")
xx1A <- xx1
yy1A <- yy1
xx3A <- xx3
yy3A <- yy3

# create X1X3 grey region and plot boundary
obsphi <- pi/24
crit <- pi/2-obsphi
theta1sA <- c(seq(-crit, crit/2, length=200), seq(crit/2, crit, length=500))
theta2sA <- asin(cos(crit)/cos(theta1sA))
theta1sB <- seq(crit, crit/2, length=500)
theta2sB <-  pi-asin(cos(crit)/cos(theta1sB))
theta1s <- c(theta1sA, theta1sB)
theta2s <- c(theta2sA, theta2sB)
vals <- k1*sin(theta1s)+k2*cos(theta1s)*cos(theta2s+pi/4)
xx1 <- cos(theta1s[vals>=0])*sin(theta2s[vals>=0]+pi/4)
yy1 <- k2*sin(theta1s[vals>=0])-k1*cos(theta1s[vals>=0])*cos(theta2s[vals>=0]+pi/4)
theta2s <-  -theta2s
vals <- k1*sin(theta1s)+k2*cos(theta1s)*cos(theta2s+pi/4)
xx3 <- cos(theta1s[vals>=0])*sin(theta2s[vals>=0]+pi/4)
yy3 <- k2*sin(theta1s[vals>=0])-k1*cos(theta1s[vals>=0])*cos(theta2s[vals>=0]+pi/4)
rev <- seq(length(xx3), 1, -1)
xx3 <- xx3[rev]
yy3 <- yy3[rev]
ang1 <-  pi+atan(xx1[length(xx1)]/yy1[length(yy1)])
ang2 <-  pi+atan(xx3[1]/yy3[1])
angs <- seq(ang1, ang2, length=50)
xx2 <- sin(angs)
yy2 <- cos(angs)
ang4 <-  pi+atan(xx1[1]/yy1[1])
ang3 <-  pi+atan(xx3[length(xx3)]/yy3[length(yy3)])
angs <- seq(ang3, ang4, length=50)
xx4 <- sin(angs)
yy4 <- cos(angs)
xxB <- c(xx1, -xx2, xx3, xx4)
yyB <- c(yy1, -yy2, yy3, yy4)
polygon(xxB, yyB, border="grey", col="grey")
xx1B <- xx1
yy1B <- yy1
xx3B <- xx3
yy3B <- yy3

# create X1X2 grey region and plot boundary
vals <- k1*sin(theta1s)+k2*cos(theta1s)*cos(theta2s-pi/4)
xx1 <- cos(theta1s[vals>=0])*sin(theta2s[vals>=0]-pi/4)
yy1 <- k2*sin(theta1s[vals>=0])-k1*cos(theta1s[vals>=0])*cos(theta2s[vals>=0]-pi/4)
theta2s <-  -theta2s
vals <- k1*sin(theta1s)+k2*cos(theta1s)*cos(theta2s-pi/4)
xx3 <- cos(theta1s[vals>=0])*sin(theta2s[vals>=0]-pi/4)
yy3 <- k2*sin(theta1s[vals>=0])-k1*cos(theta1s[vals>=0])*cos(theta2s[vals>=0]-pi/4)
rev <- seq(length(xx3), 1, -1)
xx3 <- xx3[rev]
yy3 <- yy3[rev]
ang1 <-  pi+atan(xx1[length(xx1)]/yy1[length(yy1)])
ang2 <-  pi+atan(xx3[1]/yy3[1])
angs <- seq(ang1, ang2, length=50)
xx2 <- sin(angs)
yy2 <- cos(angs)
ang4 <-  pi+atan(xx1[1]/yy1[1])
ang3 <-  pi+atan(xx3[length(xx3)]/yy3[length(yy3)])
angs <- seq(ang3, ang4, length=50)
xx4 <- sin(angs)
yy4 <- cos(angs)
xx <- c(xx1, -xx2, xx3, xx4)
yy <- c(yy1, -yy2, yy3, yy4)
polygon(xx, yy, border="grey", col="grey")
xx1C <- xx1
yy1C <- yy1
xx3C <- xx3
yy3C <- yy3


# plot boundaries to grey regions
lines(xx1C[2:45], yy1C[2:45], lwd=2)
lines(xx1C[69:583], yy1C[69:583], lwd=2)
lines(xx1C[660:1080], yy1C[660:1080], lwd=2)
lines(xx3C[13:455], yy3C[13:455], lwd=2)
lines(xx3C[538:1055], yy3C[538:1055], lwd=2)
lines(xx3C[1079:1135], yy3C[1079:1135], lwd=2)
lines(xx1A[6:113], yy1A[6:113], lwd=2)
lines(xx1A[153:346], yy1A[153:346], lwd=2)
lines(xx1A[389:484], yy1A[389:484], lwd=2)
lines(xx3A[1:93], yy3A[1:93], lwd=2)
lines(xx3A[140:362], yy3A[140:362], lwd=2)
lines(xx3A[408:497], yy3A[408:497], lwd=2)
lines(xx1B[2:45], yy1B[2:45], lwd=2)
lines(xx1B[69:583], yy1B[69:583], lwd=2)
lines(xx1B[660:1080], yy1B[660:1080], lwd=2)
lines(xx3B[13:455], yy3B[13:455], lwd=2)
lines(xx3B[538:1055], yy3B[538:1055], lwd=2)
lines(xx3B[1079:1135], yy3B[1079:1135], lwd=2)

# Plot longitudes
vals <- seq(-7, 8, 1)*pi/8
for(lambda in vals){
  sl <- sin(lambda)
  cl <- cos(lambda)
  phi <- atan(((0-1)*k2*cl)/(k1))
  angs <- seq(phi, 5.6*pi/12, length=500)
  xx <- cos(angs)*sl
  yy <- k2*sin(angs)-k1*cos(angs)*cl
  lines(xx, yy, lwd=.5)
}


# Plot Latitudes
# vals2 <- seq(-2.8, 5.6, 1.4)*pi/12
vals2 <- c(-1.5, 0, 1.5, 3.0, 4.5, 5.6)*pi/12
for(phi in vals2){
  if (k1*sin(phi) > k2 * cos(phi)) 
    crit <- pi 
  else 
    crit <- acos((-k1*sin(phi))/(k2*cos(phi)))
  lambdas <- seq(-crit, crit, length=500)
  xx <- cos(phi)*sin(lambdas)
  yy <- k2*sin(phi)-k1 * cos(phi)*cos(lambdas)
  lines(xx, yy, lwd=.5)
}


# create lines for X1X2- and X1X3-planes
lambda <- pi/4
sl <- sin(lambda)
cl <- cos(lambda)
phi <- atan(((0-1)*k2*cl)/(k1))
angs <- seq(phi, pi+phi, length=500)
xx <- cos(angs)*sl
yy <- k2*sin(angs)-k1*cos(angs)*cl
lines(xx, yy, lwd=2)
lambda <- 3*pi/4
sl <- sin(lambda)
cl <- cos(lambda)
phi <- atan(((0-1)*k2*cl)/(k1))
angs <- seq(phi, pi+phi, length=500)
xx <- cos(angs)*sl
yy <- k2*sin(angs)-k1*cos(angs)*cl
lines(xx, yy, lwd=2)

# create line for X2X3-plane
phi <- 0
crit <- acos((-k1*sin(phi))/(k2 * cos(phi)))
lambdas <- seq(-crit, crit, length=500)
xx <- cos(phi)*sin(lambdas)
yy <- k2*sin(phi)-k1 * cos(phi)*cos(lambdas)
lines(xx, yy, lwd=2)

# create axes
lines(c(0.00, 0.00), c(k2*sin(pi/2), 1.11), lwd=4)
lines(c(0.00, 0.00), c(-1, -1.12), lwd=4)
a2x <- sin(-pi/4)
a2y <- cos(-pi/4)*(-k1)
lines(c(a2x, 1.5*a2x), c(a2y, 1.5*a2y), lwd=4)
a3x <- sin(pi/4)
a3y <- cos(pi/4)*(-k1)
lines(c(a3x, 1.5*a3x), c(a3y, 1.5*a3y), lwd=4)
k <- sqrt(a3x^2+a3y^2)
lines(c(-a3x/k, 1.2*(-a3x/k)), c(-a3y/k, 1.2*(-a3y/k)), lwd=4)
k <- sqrt(a2x^2+a2y^2)
lines(c(-a2x/k, 1.2*(-a2x/k)), c(-a2y/k, 1.2*(-a2y/k)), lwd=4)


# add text
text(-1.07, -.85, expression(bold(X[2])))
text(1.11, -.85, expression(bold(X[3])))
text(0.1, 1.12, expression(bold(X[1])))

lines(XX, YY, type="l")




}
figure1.10 <- function() {


#
# Comment:
#
# Code by Steve Miller 
# (Graduate student in the Statistics Department, The University of Auckland).
#
# An example of a one-off image using the traditional graphics system.  
# All parameters are hard-coded and the image only looks right when 
# drawn with a specific aspect ratio (4:1).
#
# Also an example of drawing an empty plot with a specific coordinate system
# and then building up a final image by drawing individual lines and
# and pieces of text.
# 
# Small point of interest is the use of some special glyphs (e.g., treble
# clef) from the Hershey vector fonts.
#



# TOP: music
par(yaxt = "n", xaxt = "n", ann = F, fig = c(0, 1, 0, 1), 
    mar = c(0, 0, 0, 0), cex=0.5)
plot(1:10, type = "n", xlab = "", ylab = "")
title(main = "A Little Culture", line = -1)
E = 5; F = 5.2; G = 5.4; A = 5.6; B = 5.8; C = 6; D = 6.2; E2 = 6.4; F2 = 6.6

# stave
for (i in c(E, G, B, D, F2)) {
	lines(x = c(1, 10), y = rep(i, 2))
}

# Hershey characters (treble clef, crotchet rest, sharp)
s1 = list(x = 1.2, y = G) #place clef on G
text(list(x = c(s1$x, s1$x + 8.5, s1$x + .5), y = c(s1$y, s1$y + .4, F2)), 
     vfont = c("serif", "plain"), 
     labels = c("\\#H2330", "\\#H2378", "\\#H2323"), 
     cex = 2) 

# time signature
text(x = rep(s1$x + .3, 2), y = c(s1$y, s1$y + .8), 
     labels = c("4", "4"), cex = 0.8)

# notes
points(list(y = c(B, A, G, A, B, B, B), 
            x = c(s1$x + 1, s1$x + 2, s1$x + 3, s1$x + 4, s1$x + 5.5, 
                  s1$x + 6.5, s1$x + 7.5)), 
       pch = 16, cex = 1.2)

# note tails
tail = 1.05
for (n in c(B, A, G, A)) {
  lines(x = rep(s1$x + tail, 2), y = c(n, n + 1))
  tail = tail + 1
}

tail = tail + .5
for (n in c(B, B, B)) {
  lines(x = rep(s1$x + tail, 2), y = c(n, n + 1))
  tail = tail + 1
}

# bar lines
lines(x = rep(1, 2), y = c(E, F2))
lines(x = rep(s1$x + 4.75, 2), y = c(E, F2))
lines(x = rep(9.9, 2), y = c(E, F2))
lines(x = rep(10, 2), y = c(E, F2), lwd = 2)

# lyrics
text(x = seq(s1$x + 1, s1$x + 8.5, by = 0.5), y = rep(4, 16), 
     labels = c("Ma-", "", "ry", "", "had", "", "a", "", "", 
                "lit-", "", "tle", "", "lamb", "", ""), 
     cex = 1, font = 4)



}
figure1.11 <- function() {

#
# Comment:
#
# An example of a one-off image drawn using the grid system.
#
# The code is somewhat modular and general, with functions
# for producing different shapes, but the sizes and
# locations used in this particular image assume a 2:1 aspect ratio.
#
# The gradient-fill background (dark at the top to lighter at the 
# bottom) is achieved by filling multiple overlapping polygons with
# slowly changing shades of grey.
#


pushViewport(viewport(xscale=c(0, 1), yscale=c(0.5, 1),
             clip=TRUE))
             
res <- 50
for (i in 1:res)
  grid.rect(y=1 - (i-1)/res, just="top",
            gp=gpar(col=NULL, fill=grey(0.5*i/res)))

moon <- function(x, y, size) {
  angle <- seq(-90, 90, length=50)/180*pi
  x1 <- x + size*cos(angle)
  y1 <- y + size*sin(angle)
  mod <- 0.8
  x2 <- x + mod*(x1 - x)
  grid.polygon(c(x1, rev(x2)), c(y1, rev(y1)),
               default.unit="native",
               gp=gpar(col=NULL, fill="white"))
}

moon(.1, .9, .03)

star <- function(x, y, size) {
  x1 <- c(x,           x + size*.1, x + size*.5, x + size*.1,
          x,           x - size*.1, x - size*.5, x - size*.1) + .05
  y1 <- c(y - size,    y - size*.1, y,           y + size*.1,
          y + size*.7, y + size*.1, y,           y - size*.1) + .05
  grid.polygon(x1, y1, 
               default.unit="native",
               gp=gpar(col=NULL, fill="white"))
}

star(.5, .7, .02)
star(.8, .9, .02)
star(.72, .74, .02)
star(.62, .88, .02)

grid.circle(runif(20, .2, 1), runif(20, .6, 1), r=.002,
            default.unit="native",
            gp=gpar(col=NULL, fill="white"))

hill <- function(height=0.1, col="black") {
  n <- 100
  x <- seq(0, 1, length=n)
  y1 <- sin(runif(1) + x*2*pi)
  y2 <- sin(runif(1) + x*4*pi)
  y3 <- sin(runif(1) + x*8*pi)
  y <- 0.6 + height*((y1 + y2 + y3)/3)
  grid.polygon(c(x, rev(x)), c(y, rep(0, n)),
               default.unit="native",
               gp=gpar(col=NULL, fill=col))
}

hill()

rdir <- function(n) {
  sample(seq(-45, 45, length=10), n)/180*pi
}

grid.text("Once upon a time ...",
          x=.15, y=.51, just="bottom",
          default.unit="native",
          gp=gpar(col="white", fontface="italic", fontsize=10))

popViewport()

grid.rect()



}
figure7.1 <- function() {
library(oz)



makeImageRect <- function(nrow, ncol, cols, byrow) {
  xx <- (1:ncol)/ncol   
  yy <- (1:nrow)/nrow
  if (byrow) {
    right <- rep(xx, nrow)
    top <- rep(yy, each=ncol)
  } else {
    right <- rep(xx, each=nrow)
    top <- rep(yy, ncol)
  }  
  rectGrob(x=right, y=top, 
           width=1/ncol, height=1/nrow, 
           just=c("right", "top"), 
           gp=gpar(col=NULL, fill=cols),
           name="image")
}

imageGrob <- function(nrow, ncol, cols, byrow=TRUE,
                       name=NULL, gp=NULL, vp=NULL) { 
  igt <- gTree(nrow=nrow, ncol=ncol, 
               cols=cols, byrow=byrow,
               children=gList(makeImageRect(nrow, ncol, 
                                            cols, byrow)),
               gp=gp, name=name, vp=vp, 
               cl="imageGrob") 
  igt
}

grid.imageGrob <- function(...) {
  igt <- imageGrob(...)
  grid.draw(igt)
}


makeOzViewports <- function(ozRegion) {
  vpStack(viewport(name="ozlay", layout=grid.layout(1, 1,
                     widths=diff(ozRegion$rangex),
                     heights=diff(ozRegion$rangey), 
                     respect=TRUE)),
          viewport(name="ozvp", layout.pos.row=1, 
                   layout.pos.col=1,
                   xscale=ozRegion$rangex, 
                   yscale=ozRegion$rangey, 
                   clip=TRUE))
}

makeOzLines <- function(ozRegion) {
  numLines <- length(ozRegion$lines)
  lines <- vector("list", numLines)
  index <- 1
  for(i in ozRegion$lines) {
    lines[[index]] <- linesGrob(i$x, i$y, 
                    default.units="native",
                    vp=vpPath("ozlay", "ozvp"), 
                    name=paste("ozlines", index, sep=""))
    index <- index + 1
  }
  do.call("gList", lines)
}

ozGrob <- function(ozRegion, name=NULL, gp=NULL, vp=NULL) {
  gTree(ozRegion=ozRegion, name=name, gp=gp, vp=vp, 
    childrenvp=makeOzViewports(ozRegion), 
    children=makeOzLines(ozRegion), 
    cl="ozGrob")
}

grid.ozGrob <- function(...) {
  grid.draw(ozGrob(...))
}


ozImage <- function(mapLong, mapLat, 
                    imageLong, imageLat, cols) {
  grob(mapLong=mapLong, mapLat=mapLat, 
       imageLong=imageLong, imageLat=imageLat, cols=cols,
       cl="ozImage")  
}

drawDetails.ozImage <- function(x, recording) { 
  grid.draw(ozGrob(ozRegion(xlim=x$mapLong, 
                            ylim=x$mapLat))) 
  depth <- downViewport(vpPath("ozlay", "ozvp"))
  pushViewport(viewport(y=min(x$imageLat), 
                        height=diff(range(x$imageLat)), 
                        x=max(x$imageLong), 
                        width=diff(range(x$imageLong)),
                        default="native", 
                        just=c("right", "bottom")))
  grid.draw(imageGrob(50, 50, col=x$col)) 
  popViewport()
  upViewport(depth)
} 


calcBreaks <- function(nlevels, breaks, scale) {
  if (is.null(breaks)) {
    seq(min(scale), max(scale), diff(scale)/nlevels)
  } else {
    breaks
  }
}

ribbonVps <- function(nlevels, breaks, margin, scale) {
  breaks <- format(signif(calcBreaks(nlevels, breaks, scale), 
                          3))
  vpTree(
    viewport(name="layout", layout=
      grid.layout(3, 4,
        widths=unit.c(margin, unit(1, "lines"),
                      max(unit(0.8, "lines") + 
                          stringWidth(breaks)), margin),
        heights=unit.c(margin, unit(1, "null"), margin))),
    vpList(viewport(layout.pos.col=2, layout.pos.row=2,
                    yscale=scale, name="ribbon"),
           viewport(layout.pos.col=3, layout.pos.row=2,
                    yscale=scale, name="labels")))
}

ribbonKids <- function(nlevels, breaks, cols, scale) {
  breaks <- calcBreaks(nlevels, breaks, scale)
  nb <- length(breaks)
  tickloc <- breaks[-c(1, nb)]
  gList(rectGrob(y=unit(breaks[-1], "native"), 
                 height=unit(diff(breaks), "native"),
                 just="top", gp=gpar(fill=cols),
                 vp=vpPath("layout", "ribbon")),
        segmentsGrob(x1=unit(0.5, "lines"),
                     y0=unit(tickloc, "native"),
                     y1=unit(tickloc, "native"),
                     vp=vpPath("layout", "labels")),
        textGrob(x=unit(0.8, "lines"),
                 y=unit(tickloc, "native"),
                 just="left", 
                 label=format(signif(tickloc, 3)),
                 vp=vpPath("layout", "labels")))
}


ribbonLegend <- function(nlevels=NULL, breaks=NULL, cols, 
                         scale=range(breaks), 
                         margin=unit(0.5, "lines"), 
                         gp=NULL, vp=NULL, name=NULL) {
  gTree(
    nlevels=nlevels, breaks=breaks, cols=cols, scale=scale, 
    children=ribbonKids(nlevels, breaks, cols, scale),
    childrenvp=ribbonVps(nlevels, breaks, margin, scale),
    gp=gp, vp=vp, name=name, cl="ribbonLegend")
}

widthDetails.ribbonLegend <- function(x) { 
  sum(layout.widths(viewport.layout(x$childrenvp[[1]]))) 
} 


mapLong <- c(132, 136)
mapLat <- c(-35, -31.5)
imageLong <- range(fluoro.predict$x)
imageLat <- range(fluoro.predict$y)
zbreaks <- seq(min(fluoro.predict$z, na.rm=TRUE), 
               max(fluoro.predict$z, na.rm=TRUE), 
               length=10)
zcol <- cut(fluoro.predict$z, zbreaks,
            include.lowest=TRUE, labels=FALSE)
ozgreys <- grey(0.5 + 1:9/20)
imageCols <- ozgreys[zcol]



ozKey <- function(x, y, width, height, just, 
                  mapLong, mapLat) {
  gTree(childrenvp=viewport(name="ozkeyframe",
                            x=x, y=y, just=just,
                            width=width, height=height),
        children=gList(ozGrob(ozRegion(), vp="ozkeyframe",
                              gp=gpar(lwd=0.1)),
                       rectGrob(x=mean(mapLong),
                                y=mean(mapLat),
                                width=abs(diff(mapLong)),
                                height=abs(diff(mapLat)),
                                default.units="native",
                                gp=gpar(lwd=1),
                                vp=vpPath("ozkeyframe",
                                          "ozlay", "ozvp"))))
}


ozimage <- ozImage(mapLong, mapLat, 
                   imageLong, imageLat, imageCols)



ribbonlegend <- ribbonLegend(breaks=zbreaks, 
                             cols=ozgreys, 
                             scale=range(zbreaks),
                             gp=gpar(cex=0.7))



ozkey <- ozKey(x=unit(1, "npc") - unit(1, "mm"),
               y=unit(1, "npc") - unit(1, "mm"),
               width=unit(3.5, "cm"),
               height=unit(2, "cm"),
               just=c("right", "top"),
               mapLong, mapLat)



grid.rect(gp=gpar(col="grey"))
fg <- frameGrob()
fg <- packGrob(fg, ozimage)
fg <- placeGrob(fg, ozkey)
fg <- packGrob(fg, ribbonlegend, "right")
grid.draw(fg)




}
grid.imageFun <- function(nrow, ncol, cols, 
                          byrow=TRUE) {
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  if (byrow) {
    right <- rep(x, nrow)
    top <- rep(y, each=ncol)
  } else {
    right <- rep(x, each=nrow)
    top <- rep(y, ncol)
  }
  grid.rect(x=right, y=top,  
    width=1/ncol, height=1/nrow, 
    just=c("right", "top"),
    gp=gpar(col=NA, fill=cols),
    name="image") 
}


figure7.3 <- function() {
greys <- grey(0.5 + (rep(1:4, 4) - rep(0:3, each=4))/10)



grid.imageFun <- function(nrow, ncol, cols, 
                          byrow=TRUE) {
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  if (byrow) {
    right <- rep(x, nrow)
    top <- rep(y, each=ncol)
  } else {
    right <- rep(x, each=nrow)
    top <- rep(y, ncol)
  }
  grid.rect(x=right, y=top,  
    width=1/ncol, height=1/nrow, 
    just=c("right", "top"),
    gp=gpar(col=NA, fill=cols),
    name="image") 
}


pushViewport(viewport(layout=grid.layout(3, 5, widths=c(1,8,2,8,1),
  heights=unit(c(1, 8, 1), c("null", "null", "lines")))))
pushViewport(viewport(layout.pos.col=2, 
                      layout.pos.row=2))
grid.imageFun(4, 4, greys)

popViewport()
pushViewport(viewport(layout.pos.col=2, 
                      layout.pos.row=3))
grid.text("(a)", gp=gpar(cex=0.7))
popViewport()
pushViewport(viewport(layout.pos.col=4,
                      layout.pos.row=2))
grid.imageFun(4, 4, greys, byrow=FALSE)

popViewport()
pushViewport(viewport(layout.pos.col=4, 
                      layout.pos.row=3))
grid.text("(b)", gp=gpar(cex=0.7))
popViewport(2)



}
grid.ozFun <- function(ozRegion) {
  pushViewport( 
    viewport(name="ozlay", 
             layout=grid.layout(1,1,
                      widths=diff(ozRegion$rangex),
                      heights=diff(ozRegion$rangey), 
                      respect=TRUE)))
  pushViewport(viewport(name="ozvp", 
                        layout.pos.row=1, 
                        layout.pos.col=1,
                        xscale=ozRegion$rangex, 
                        yscale=ozRegion$rangey, 
                        clip=TRUE)) 
  index <- 1
  for(i in ozRegion$lines) {
    grid.lines(i$x, i$y, default.units="native",
               name=paste("ozlines", index, sep="")) 
    index <- index + 1
  }
  upViewport(2) 
}


figure7.5 <- function() {
grid.ozFun <- function(ozRegion) {
  pushViewport( 
    viewport(name="ozlay", 
             layout=grid.layout(1,1,
                      widths=diff(ozRegion$rangex),
                      heights=diff(ozRegion$rangey), 
                      respect=TRUE)))
  pushViewport(viewport(name="ozvp", 
                        layout.pos.row=1, 
                        layout.pos.col=1,
                        xscale=ozRegion$rangex, 
                        yscale=ozRegion$rangey, 
                        clip=TRUE)) 
  index <- 1
  for(i in ozRegion$lines) {
    grid.lines(i$x, i$y, default.units="native",
               name=paste("ozlines", index, sep="")) 
    index <- index + 1
  }
  upViewport(2) 
}


grid.rect(gp=gpar(col="grey"))
grid.ozFun(ozRegion())




}
figure7.6 <- function() {
library(oz)



grid.imageFun <- function(nrow, ncol, cols, 
                          byrow=TRUE) {
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  if (byrow) {
    right <- rep(x, nrow)
    top <- rep(y, each=ncol)
  } else {
    right <- rep(x, each=nrow)
    top <- rep(y, ncol)
  }
  grid.rect(x=right, y=top,  
    width=1/ncol, height=1/nrow, 
    just=c("right", "top"),
    gp=gpar(col=NA, fill=cols),
    name="image") 
}


grid.ozFun <- function(ozRegion) {
  pushViewport( 
    viewport(name="ozlay", 
             layout=grid.layout(1,1,
                      widths=diff(ozRegion$rangex),
                      heights=diff(ozRegion$rangey), 
                      respect=TRUE)))
  pushViewport(viewport(name="ozvp", 
                        layout.pos.row=1, 
                        layout.pos.col=1,
                        xscale=ozRegion$rangex, 
                        yscale=ozRegion$rangey, 
                        clip=TRUE)) 
  index <- 1
  for(i in ozRegion$lines) {
    grid.lines(i$x, i$y, default.units="native",
               name=paste("ozlines", index, sep="")) 
    index <- index + 1
  }
  upViewport(2) 
}


mapLong <- c(132, 136)
mapLat <- c(-35, -31.5)
imageLong <- range(fluoro.predict$x)
imageLat <- range(fluoro.predict$y)
zbreaks <- seq(min(fluoro.predict$z, na.rm=TRUE), 
               max(fluoro.predict$z, na.rm=TRUE), 
               length=10)
zcol <- cut(fluoro.predict$z, zbreaks,
            include.lowest=TRUE, labels=FALSE)
ozgreys <- grey(0.5 + 1:9/20)
imageCols <- ozgreys[zcol]



mapLong <- c(132, 136)
mapLat <- c(-35, -31.5)
imageLong <- range(fluoro.predict$x)
imageLat <- range(fluoro.predict$y)
zbreaks <- seq(min(fluoro.predict$z, na.rm=TRUE), 
               max(fluoro.predict$z, na.rm=TRUE), 
               length=10)
zcol <- cut(fluoro.predict$z, zbreaks,
            include.lowest=TRUE, labels=FALSE)
ozgreys <- grey(0.5 + 1:9/20)
imageCols <- ozgreys[zcol]

grid.rect(gp=gpar(col="grey"))
grid.ozFun(ozRegion(xlim=mapLong, ylim=mapLat))

downViewport("ozvp")

pushViewport(viewport(y=min(imageLat), 
                      height=abs(diff(imageLat)), 
                      x=max(imageLong), 
                      width=abs(diff(imageLong)),
                      default.units="native", 
                      just=c("right", "bottom")))
grid.imageFun(50, 50, col=imageCols)
upViewport(0)




}
figure7.7 <- function() {
library(oz)



grid.imageFun <- function(nrow, ncol, cols, 
                          byrow=TRUE) {
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  if (byrow) {
    right <- rep(x, nrow)
    top <- rep(y, each=ncol)
  } else {
    right <- rep(x, each=nrow)
    top <- rep(y, ncol)
  }
  grid.rect(x=right, y=top,  
    width=1/ncol, height=1/nrow, 
    just=c("right", "top"),
    gp=gpar(col=NA, fill=cols),
    name="image") 
}


grid.ozFun <- function(ozRegion) {
  pushViewport( 
    viewport(name="ozlay", 
             layout=grid.layout(1,1,
                      widths=diff(ozRegion$rangex),
                      heights=diff(ozRegion$rangey), 
                      respect=TRUE)))
  pushViewport(viewport(name="ozvp", 
                        layout.pos.row=1, 
                        layout.pos.col=1,
                        xscale=ozRegion$rangex, 
                        yscale=ozRegion$rangey, 
                        clip=TRUE)) 
  index <- 1
  for(i in ozRegion$lines) {
    grid.lines(i$x, i$y, default.units="native",
               name=paste("ozlines", index, sep="")) 
    index <- index + 1
  }
  upViewport(2) 
}


mapLong <- c(132, 136)
mapLat <- c(-35, -31.5)
imageLong <- range(fluoro.predict$x)
imageLat <- range(fluoro.predict$y)
zbreaks <- seq(min(fluoro.predict$z, na.rm=TRUE), 
               max(fluoro.predict$z, na.rm=TRUE), 
               length=10)
zcol <- cut(fluoro.predict$z, zbreaks,
            include.lowest=TRUE, labels=FALSE)
ozgreys <- grey(0.5 + 1:9/20)
imageCols <- ozgreys[zcol]



grid.rect(gp=gpar(col="grey"))
grid.ozFun(ozRegion(xlim=mapLong, ylim=mapLat))

downViewport("ozvp")

pushViewport(viewport(y=min(imageLat), 
                      height=abs(diff(imageLat)), 
                      x=max(imageLong), 
                      width=abs(diff(imageLong)),
                      default.units="native", 
                      just=c("right", "bottom")))
grid.imageFun(50, 50, col=imageCols)
upViewport(0)

grid.edit("image", gp=gpar(fill=rev(ozgreys)[zcol]))
grid.edit("^ozlines[0-9]+$", gp=gpar(col="grey", lwd=2), 
          grep=TRUE, global=TRUE)




}
makeImageRect <- function(nrow, ncol, cols, byrow) {
  xx <- (1:ncol)/ncol   
  yy <- (1:nrow)/nrow
  if (byrow) {
    right <- rep(xx, nrow)
    top <- rep(yy, each=ncol)
  } else {
    right <- rep(xx, each=nrow)
    top <- rep(yy, ncol)
  }  
  rectGrob(x=right, y=top, 
           width=1/ncol, height=1/nrow, 
           just=c("right", "top"), 
           gp=gpar(col=NULL, fill=cols),
           name="image")
}

imageGrob <- function(nrow, ncol, cols, byrow=TRUE,
                       name=NULL, gp=NULL, vp=NULL) { 
  igt <- gTree(nrow=nrow, ncol=ncol, 
               cols=cols, byrow=byrow,
               children=gList(makeImageRect(nrow, ncol, 
                                            cols, byrow)),
               gp=gp, name=name, vp=vp, 
               cl="imageGrob") 
  igt
}

grid.imageGrob <- function(...) {
  igt <- imageGrob(...)
  grid.draw(igt)
}


validDetails.imageGrob <- function(x) { 
  if (!is.numeric(x$nrow) || length(x$nrow) > 1 || 
      !is.numeric(x$ncol) || length(x$ncol) > 1)
    stop("nrow and ncol must be numeric and length 1")
  if (!is.logical(x$byrow))
    stop("byrow must be logical")
  x 
} 

validDetails.ozGrob <- function(x) {
  if (!inherits(x$ozRegion, "ozRegion"))
    stop("Invalid ozRegion")
  x
}


makeOzViewports <- function(ozRegion) {
  vpStack(viewport(name="ozlay", layout=grid.layout(1, 1,
                     widths=diff(ozRegion$rangex),
                     heights=diff(ozRegion$rangey), 
                     respect=TRUE)),
          viewport(name="ozvp", layout.pos.row=1, 
                   layout.pos.col=1,
                   xscale=ozRegion$rangex, 
                   yscale=ozRegion$rangey, 
                   clip=TRUE))
}

makeOzLines <- function(ozRegion) {
  numLines <- length(ozRegion$lines)
  lines <- vector("list", numLines)
  index <- 1
  for(i in ozRegion$lines) {
    lines[[index]] <- linesGrob(i$x, i$y, 
                    default.units="native",
                    vp=vpPath("ozlay", "ozvp"), 
                    name=paste("ozlines", index, sep=""))
    index <- index + 1
  }
  do.call("gList", lines)
}

ozGrob <- function(ozRegion, name=NULL, gp=NULL, vp=NULL) {
  gTree(ozRegion=ozRegion, name=name, gp=gp, vp=vp, 
    childrenvp=makeOzViewports(ozRegion), 
    children=makeOzLines(ozRegion), 
    cl="ozGrob")
}

grid.ozGrob <- function(...) {
  grid.draw(ozGrob(...))
}


ozImage <- function(mapLong, mapLat, 
                    imageLong, imageLat, cols) {
  grob(mapLong=mapLong, mapLat=mapLat, 
       imageLong=imageLong, imageLat=imageLat, cols=cols,
       cl="ozImage")  
}

drawDetails.ozImage <- function(x, recording) { 
  grid.draw(ozGrob(ozRegion(xlim=x$mapLong, 
                            ylim=x$mapLat))) 
  depth <- downViewport(vpPath("ozlay", "ozvp"))
  pushViewport(viewport(y=min(x$imageLat), 
                        height=diff(range(x$imageLat)), 
                        x=max(x$imageLong), 
                        width=diff(range(x$imageLong)),
                        default="native", 
                        just=c("right", "bottom")))
  grid.draw(imageGrob(50, 50, col=x$col)) 
  popViewport()
  upViewport(depth)
} 


editDetails.imageGrob <- function(x, specs) { 
  if (any(c("ncol", "nrow", "byrow") %in% names(specs))) { 
    x <- addGrob(x, makeImageRect(x$nrow, x$ncol,
                                  x$cols, x$byrow))
  } 
  if (any(c("cols") %in% names(specs))) { 
    x <- editGrob(x, "image", gp=gpar(fill=x$cols))
  } 
  x 
} 

editDetails.ozGrob <- function(x, specs) {
  if ("ozRegion" %in% names(specs)) {
    x$childrenvp <- makeOzViewports(x$ozRegion)
    x <- setChildren(x, makeOzLines(x$ozRegion))
  }
  x
}


figure7.13 <- function() {
greys <- grey(0.5 + (rep(1:4, 4) - rep(0:3, each=4))/10)



makeImageRect <- function(nrow, ncol, cols, byrow) {
  xx <- (1:ncol)/ncol   
  yy <- (1:nrow)/nrow
  if (byrow) {
    right <- rep(xx, nrow)
    top <- rep(yy, each=ncol)
  } else {
    right <- rep(xx, each=nrow)
    top <- rep(yy, ncol)
  }  
  rectGrob(x=right, y=top, 
           width=1/ncol, height=1/nrow, 
           just=c("right", "top"), 
           gp=gpar(col=NULL, fill=cols),
           name="image")
}

imageGrob <- function(nrow, ncol, cols, byrow=TRUE,
                       name=NULL, gp=NULL, vp=NULL) { 
  igt <- gTree(nrow=nrow, ncol=ncol, 
               cols=cols, byrow=byrow,
               children=gList(makeImageRect(nrow, ncol, 
                                            cols, byrow)),
               gp=gp, name=name, vp=vp, 
               cl="imageGrob") 
  igt
}

grid.imageGrob <- function(...) {
  igt <- imageGrob(...)
  grid.draw(igt)
}


editDetails.imageGrob <- function(x, specs) { 
  if (any(c("ncol", "nrow", "byrow") %in% names(specs))) { 
    x <- addGrob(x, makeImageRect(x$nrow, x$ncol,
                                  x$cols, x$byrow))
  } 
  if (any(c("cols") %in% names(specs))) { 
    x <- editGrob(x, "image", gp=gpar(fill=x$cols))
  } 
  x 
} 

editDetails.ozGrob <- function(x, specs) {
  if ("ozRegion" %in% names(specs)) {
    x$childrenvp <- makeOzViewports(x$ozRegion)
    x <- setChildren(x, makeOzLines(x$ozRegion))
  }
  x
}


pushViewport(viewport(layout=grid.layout(2, 1, 
                                         heights=unit(c(1, 1),
                                                      c("null", "lines")),
                                         respect=TRUE)))
pushViewport(viewport(layout.pos.row=1))
grid.imageGrob(4, 4, greys, name="imageGrob")

popViewport()
pushViewport(viewport(layout.pos.row=2, gp=gpar(cex=0.7)))
grid.text("(a)", name="label")
popViewport()
grid.edit("imageGrob", byrow=FALSE)

grid.remove("label")
pushViewport(viewport(layout.pos.row=2, gp=gpar(cex=0.7)))
grid.text("(b)", name="label")
popViewport()
grid.edit("imageGrob::image", gp=gpar(col="white", lwd=6))

grid.remove("label")
pushViewport(viewport(layout.pos.row=2, gp=gpar(cex=0.7)))
grid.text("(c)", name="label")
popViewport(2)



}
figure7.14 <- function() {
greys <- grey(0.5 + (rep(1:4, 4) - rep(0:3, each=4))/10)



makeImageRect <- function(nrow, ncol, cols, byrow) {
  xx <- (1:ncol)/ncol   
  yy <- (1:nrow)/nrow
  if (byrow) {
    right <- rep(xx, nrow)
    top <- rep(yy, each=ncol)
  } else {
    right <- rep(xx, each=nrow)
    top <- rep(yy, ncol)
  }  
  rectGrob(x=right, y=top, 
           width=1/ncol, height=1/nrow, 
           just=c("right", "top"), 
           gp=gpar(col=NULL, fill=cols),
           name="image")
}

imageGrob <- function(nrow, ncol, cols, byrow=TRUE,
                       name=NULL, gp=NULL, vp=NULL) { 
  igt <- gTree(nrow=nrow, ncol=ncol, 
               cols=cols, byrow=byrow,
               children=gList(makeImageRect(nrow, ncol, 
                                            cols, byrow)),
               gp=gp, name=name, vp=vp, 
               cl="imageGrob") 
  igt
}

grid.imageGrob <- function(...) {
  igt <- imageGrob(...)
  grid.draw(igt)
}


editDetails.imageGrob <- function(x, specs) { 
  if (any(c("ncol", "nrow", "byrow") %in% names(specs))) { 
    x <- addGrob(x, makeImageRect(x$nrow, x$ncol,
                                  x$cols, x$byrow))
  } 
  if (any(c("cols") %in% names(specs))) { 
    x <- editGrob(x, "image", gp=gpar(fill=x$cols))
  } 
  x 
} 

editDetails.ozGrob <- function(x, specs) {
  if ("ozRegion" %in% names(specs)) {
    x$childrenvp <- makeOzViewports(x$ozRegion)
    x <- setChildren(x, makeOzLines(x$ozRegion))
  }
  x
}


pushViewport(viewport(layout=grid.layout(2, 1, 
                                         heights=unit(c(1, 1),
                                                      c("null", "lines")),
                                         respect=TRUE)))
pushViewport(viewport(layout.pos.row=1))
grid.imageGrob(4, 4, greys, name="imageGrob")

grid.edit("imageGrob::image", gp=gpar(col="white"))

popViewport()
pushViewport(viewport(layout.pos.row=2, gp=gpar(cex=0.7)))
grid.text("(a)", name="label")
popViewport()
grid.edit("imageGrob", cols=rev(greys))

grid.remove("label")
pushViewport(viewport(layout.pos.row=2, gp=gpar(cex=0.7)))
grid.text("(b)", name="label")
popViewport()
grid.edit("imageGrob", byrow=FALSE)

grid.remove("label")
pushViewport(viewport(layout.pos.row=2, gp=gpar(cex=0.7)))
grid.text("(c)", name="label")
popViewport(2)



}
calcBreaks <- function(nlevels, breaks, scale) {
  if (is.null(breaks)) {
    seq(min(scale), max(scale), diff(scale)/nlevels)
  } else {
    breaks
  }
}

ribbonVps <- function(nlevels, breaks, margin, scale) {
  breaks <- format(signif(calcBreaks(nlevels, breaks, scale), 
                          3))
  vpTree(
    viewport(name="layout", layout=
      grid.layout(3, 4,
        widths=unit.c(margin, unit(1, "lines"),
                      max(unit(0.8, "lines") + 
                          stringWidth(breaks)), margin),
        heights=unit.c(margin, unit(1, "null"), margin))),
    vpList(viewport(layout.pos.col=2, layout.pos.row=2,
                    yscale=scale, name="ribbon"),
           viewport(layout.pos.col=3, layout.pos.row=2,
                    yscale=scale, name="labels")))
}

ribbonKids <- function(nlevels, breaks, cols, scale) {
  breaks <- calcBreaks(nlevels, breaks, scale)
  nb <- length(breaks)
  tickloc <- breaks[-c(1, nb)]
  gList(rectGrob(y=unit(breaks[-1], "native"), 
                 height=unit(diff(breaks), "native"),
                 just="top", gp=gpar(fill=cols),
                 vp=vpPath("layout", "ribbon")),
        segmentsGrob(x1=unit(0.5, "lines"),
                     y0=unit(tickloc, "native"),
                     y1=unit(tickloc, "native"),
                     vp=vpPath("layout", "labels")),
        textGrob(x=unit(0.8, "lines"),
                 y=unit(tickloc, "native"),
                 just="left", 
                 label=format(signif(tickloc, 3)),
                 vp=vpPath("layout", "labels")))
}


ribbonLegend <- function(nlevels=NULL, breaks=NULL, cols, 
                         scale=range(breaks), 
                         margin=unit(0.5, "lines"), 
                         gp=NULL, vp=NULL, name=NULL) {
  gTree(
    nlevels=nlevels, breaks=breaks, cols=cols, scale=scale, 
    children=ribbonKids(nlevels, breaks, cols, scale),
    childrenvp=ribbonVps(nlevels, breaks, margin, scale),
    gp=gp, vp=vp, name=name, cl="ribbonLegend")
}

widthDetails.ribbonLegend <- function(x) { 
  sum(layout.widths(viewport.layout(x$childrenvp[[1]]))) 
} 


ozKey <- function(x, y, width, height, just, 
                  mapLong, mapLat) {
  gTree(childrenvp=viewport(name="ozkeyframe",
                            x=x, y=y, just=just,
                            width=width, height=height),
        children=gList(ozGrob(ozRegion(), vp="ozkeyframe",
                              gp=gpar(lwd=0.1)),
                       rectGrob(x=mean(mapLong),
                                y=mean(mapLat),
                                width=abs(diff(mapLong)),
                                height=abs(diff(mapLat)),
                                default.units="native",
                                gp=gpar(lwd=1),
                                vp=vpPath("ozkeyframe",
                                          "ozlay", "ozvp"))))
}


figure7.18 <- function() {
library(oz)



makeOzViewports <- function(ozRegion) {
  vpStack(viewport(name="ozlay", layout=grid.layout(1, 1,
                     widths=diff(ozRegion$rangex),
                     heights=diff(ozRegion$rangey), 
                     respect=TRUE)),
          viewport(name="ozvp", layout.pos.row=1, 
                   layout.pos.col=1,
                   xscale=ozRegion$rangex, 
                   yscale=ozRegion$rangey, 
                   clip=TRUE))
}

makeOzLines <- function(ozRegion) {
  numLines <- length(ozRegion$lines)
  lines <- vector("list", numLines)
  index <- 1
  for(i in ozRegion$lines) {
    lines[[index]] <- linesGrob(i$x, i$y, 
                    default.units="native",
                    vp=vpPath("ozlay", "ozvp"), 
                    name=paste("ozlines", index, sep=""))
    index <- index + 1
  }
  do.call("gList", lines)
}

ozGrob <- function(ozRegion, name=NULL, gp=NULL, vp=NULL) {
  gTree(ozRegion=ozRegion, name=name, gp=gp, vp=vp, 
    childrenvp=makeOzViewports(ozRegion), 
    children=makeOzLines(ozRegion), 
    cl="ozGrob")
}

grid.ozGrob <- function(...) {
  grid.draw(ozGrob(...))
}


calcBreaks <- function(nlevels, breaks, scale) {
  if (is.null(breaks)) {
    seq(min(scale), max(scale), diff(scale)/nlevels)
  } else {
    breaks
  }
}

ribbonVps <- function(nlevels, breaks, margin, scale) {
  breaks <- format(signif(calcBreaks(nlevels, breaks, scale), 
                          3))
  vpTree(
    viewport(name="layout", layout=
      grid.layout(3, 4,
        widths=unit.c(margin, unit(1, "lines"),
                      max(unit(0.8, "lines") + 
                          stringWidth(breaks)), margin),
        heights=unit.c(margin, unit(1, "null"), margin))),
    vpList(viewport(layout.pos.col=2, layout.pos.row=2,
                    yscale=scale, name="ribbon"),
           viewport(layout.pos.col=3, layout.pos.row=2,
                    yscale=scale, name="labels")))
}

ribbonKids <- function(nlevels, breaks, cols, scale) {
  breaks <- calcBreaks(nlevels, breaks, scale)
  nb <- length(breaks)
  tickloc <- breaks[-c(1, nb)]
  gList(rectGrob(y=unit(breaks[-1], "native"), 
                 height=unit(diff(breaks), "native"),
                 just="top", gp=gpar(fill=cols),
                 vp=vpPath("layout", "ribbon")),
        segmentsGrob(x1=unit(0.5, "lines"),
                     y0=unit(tickloc, "native"),
                     y1=unit(tickloc, "native"),
                     vp=vpPath("layout", "labels")),
        textGrob(x=unit(0.8, "lines"),
                 y=unit(tickloc, "native"),
                 just="left", 
                 label=format(signif(tickloc, 3)),
                 vp=vpPath("layout", "labels")))
}


ribbonLegend <- function(nlevels=NULL, breaks=NULL, cols, 
                         scale=range(breaks), 
                         margin=unit(0.5, "lines"), 
                         gp=NULL, vp=NULL, name=NULL) {
  gTree(
    nlevels=nlevels, breaks=breaks, cols=cols, scale=scale, 
    children=ribbonKids(nlevels, breaks, cols, scale),
    childrenvp=ribbonVps(nlevels, breaks, margin, scale),
    gp=gp, vp=vp, name=name, cl="ribbonLegend")
}

widthDetails.ribbonLegend <- function(x) { 
  sum(layout.widths(viewport.layout(x$childrenvp[[1]]))) 
} 


grid.ozGrob(ozRegion())
downViewport("ozvp")
for (i in 1:(dim(ozTemp)[1])) {
  grid.points(ozTemp$long[i], ozTemp$lat[i], pch=16)
  rl <- ribbonLegend(breaks=c(min(ozTemp$min), 
                              ozTemp$min[i], 
                              ozTemp$max[i], 
                              max(ozTemp$max)),
                     cols=c("white", "grey", "white"))
  pushViewport(viewport(x=unit(ozTemp$long[i], "native"),
                        y=unit(ozTemp$lat[i], "native"),
                        height=unit(1, "inches"),
                        width=grobWidth(rl),
                        clip="off"))
  grid.circle(r=0.7, 
              gp=gpar(col="grey", fill="white", alpha=0.8))
  grid.draw(rl)
  popViewport()
}
upViewport(0)



}
splitString <- function(text) {
  strings <- strsplit(text, " ")[[1]]
  newstring <- strings[1]
  linewidth <- stringWidth(newstring)
  gapwidth <- stringWidth(" ")
  availwidth <- 
    convertWidth(unit(1, "npc"), 
                 "inches", valueOnly=TRUE) 
  for (i in 2:length(strings)) {
    width <- stringWidth(strings[i])
    if (convertWidth(linewidth + gapwidth + width, 
                     "inches", valueOnly=TRUE) <
        availwidth) {
      sep <- " "
      linewidth <- linewidth + gapwidth + width
    } else {
      sep <- "\n"
      linewidth <- width
    }
    newstring <- paste(newstring, strings[i], sep=sep)
  }
  newstring
}   


figure7.20 <- function() {
splitString <- function(text) {
  strings <- strsplit(text, " ")[[1]]
  newstring <- strings[1]
  linewidth <- stringWidth(newstring)
  gapwidth <- stringWidth(" ")
  availwidth <- 
    convertWidth(unit(1, "npc"), 
                 "inches", valueOnly=TRUE) 
  for (i in 2:length(strings)) {
    width <- stringWidth(strings[i])
    if (convertWidth(linewidth + gapwidth + width, 
                     "inches", valueOnly=TRUE) <
        availwidth) {
      sep <- " "
      linewidth <- linewidth + gapwidth + width
    } else {
      sep <- "\n"
      linewidth <- width
    }
    newstring <- paste(newstring, strings[i], sep=sep)
  }
  newstring
}   


text <- "The quick brown fox jumps over the lazy dog."
grid.text(splitString(text), 
          x=0, y=1, just=c("left", "top")) 



splitTextGrob <- function(text, ...) {
  grob(text=text, cl="splitText", ...)
}

drawDetails.splitText <- function(x, recording) {
  grid.text(splitString(x$text),
            x=0, y=1, just=c("left", "top")) 
}


pushViewport(viewport(layout=grid.layout(2, 2)))
pushViewport(viewport(layout.pos.col=1))
pushViewport(viewport(width=0.5, height=0.9))
grid.rect(gp=gpar(col="grey"))
text <- "The quick brown fox jumps over the lazy dog."
grid.text(splitString(text), 
          x=0, y=1, just=c("left", "top")) 

popViewport(2)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
pushViewport(viewport(height=0.8))
grid.rect(gp=gpar(col="grey"))
splitText <- splitTextGrob(text, name="splitText")
grid.draw(splitText)

popViewport(2)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
pushViewport(viewport(height=0.8))
grid.rect(gp=gpar(col="grey"))
grid.draw(editGrob(splitText, gp=gpar(cex=1.5)))
popViewport(2)
popViewport()



}
splitTextGrob <- function(text, ...) {
  grob(text=text, cl="splitText", ...)
}

drawDetails.splitText <- function(x, recording) {
  grid.text(splitString(x$text),
            x=0, y=1, just=c("left", "top")) 
}


figure7.22 <- function() {
faceA <- function(x, y, width, height) {
  pushViewport(viewport(x=x, y=y, 
                        width=width, height=height))
  grid.rect()
  grid.circle(x=c(0.25, 0.75), y=0.75, r=0.1)
  grid.lines(x=c(0.33, 0.67), y=0.25)
  popViewport()
}

faceB <- function(x, y, width, height) {
  pushViewport(viewport(x=x, y=y, 
                        width=width, height=height))
  grid.draw(rectGrob())
  grid.draw(circleGrob(x=c(0.25, 0.75), y=0.75, r=0.1))
  grid.draw(linesGrob(x=c(0.33, 0.67), y=0.25))
  popViewport()
}


faceA(.5, .5, width=.1, height=.1)
angle <- seq(0, 2*pi, length=9)[-9]
for (i in angle) {
  x <- 0.5 + 0.3*cos(i)
  y <- 0.5 + 0.3*sin(i)
  faceA(x, y, 0.2*x, 0.2*y)
}
grid.rect(width=.9, height=.9, gp=gpar(col="grey"))



}
faceA <- function(x, y, width, height) {
  pushViewport(viewport(x=x, y=y, 
                        width=width, height=height))
  grid.rect()
  grid.circle(x=c(0.25, 0.75), y=0.75, r=0.1)
  grid.lines(x=c(0.33, 0.67), y=0.25)
  popViewport()
}

faceB <- function(x, y, width, height) {
  pushViewport(viewport(x=x, y=y, 
                        width=width, height=height))
  grid.draw(rectGrob())
  grid.draw(circleGrob(x=c(0.25, 0.75), y=0.75, r=0.1))
  grid.draw(linesGrob(x=c(0.33, 0.67), y=0.25))
  popViewport()
}


faceC <- function(x, y, width, height) {
  gTree(childrenvp=viewport(x=x, y=y,
                            width=width, height=height,
                            name="face"),
        children=gList(rectGrob(vp="face"),
                       circleGrob(x=c(0.25, 0.75), 
                                  y=0.75, r=0.1, vp="face"),
                       linesGrob(x=c(0.33, 0.67), y=0.25,
                                 vp="face")))
}

faceD <- function(x, y, width, height) {
  grid.grabExpr({
                  pushViewport(viewport(x=x, y=y,
                                        width=size, 
                                        height=size))
                  grid.rect()
                  grid.circle(x=c(0.25, 0.75), 
                              y=0.75, r=0.1)
                  grid.lines(x=c(0.33, 0.67), y=0.25)
                  popViewport()
                })
}

drawDetails.face <- function(x, recording) {
  pushViewport(viewport(x=x$x, y=x$y,
                        width=x$width, height=x$height))
  grid.rect()
  grid.circle(x=c(0.25, 0.75), y=0.75, r=0.1)
  grid.lines(x=c(0.33, 0.67), y=0.25)
  popViewport()  
}

faceE <- function(x, y, width, height) {
  grob(x=x, y=y, width=width, height=height, cl="face")
}


figure1.1 <- function() {
plot(pressure)
text(150, 600, 
     "Pressure (mm Hg)\nversus\nTemperature (Celsius)")



}
figure6.1 <- function() {
grid.rect(gp=gpar(col="grey"))
grid.circle(name="circles", x=seq(0.1, 0.9, length=40), 
            y=0.5 + 0.4*sin(seq(0, 2*pi, length=40)),
            r=abs(0.1*cos(seq(0, 2*pi, length=40))))

grid.edit("circles", 
          gp=gpar(col=grey(c(1:20*0.04, 20:1*0.04))))

grid.remove("circles")




}
figure6.2 <- function() {
grid.rect(gp=gpar(col="grey"))
suffix <- c("even", "odd")
for (i in 1:8)
  grid.circle(name=paste("circle.", suffix[i %% 2 + 1], 
                         sep=""),
              r=(9 - i)/20, 
              gp=gpar(col=NA, fill=grey(i/10)))

grid.edit("circle.odd", gp=gpar(fill="grey10"), 
          global=TRUE)

grid.edit("circle", gp=gpar(col="grey", fill="grey90"), 
          grep=TRUE, global=TRUE) 




}
figure6.3 <- function() {
labels <- c("\"xaxis1\"\nxaxis gTree", "\"major\"\nlines grob", 
            "\"ticks\"\nlines grob", "\"labels\"\ntext grob")
names <- c("", "major", "ticks", "labels")
boxheight <- unit(2.5, "lines")
boxwidth <- unit(1.2, "inches")
pushViewport(viewport(layout=grid.layout(2, 3)))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.text(labels[1])
grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
           0.5, gp=gpar(col="grey"))
grid.roundRect(height=boxheight, 
               width=boxwidth, # 1.2*stringWidth(labels[1]),
               r=unit(2, "mm"))
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
grid.text(labels[2])
grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
           0.5, gp=gpar(col="grey"))
grid.roundRect(height=boxheight, 
               width=boxwidth, # 1.2*stringWidth(labels[2]),
               r=unit(2, "mm"))
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
grid.text(labels[3])
grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
           0.5, gp=gpar(col="grey"))
grid.roundRect(height=boxheight, 
               width=boxwidth, # 1.2*stringWidth(labels[3]),
               r=unit(2, "mm"))
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=3))
grid.text(labels[4])
grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
           0.5, gp=gpar(col="grey"))
grid.roundRect(height=boxheight, 
               width=boxwidth, # 1.2*stringWidth(labels[4]),
               r=unit(2, "mm"))
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.move.to(x=0.5, y=unit(0.5, "npc") - 0.5*boxheight)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
grid.arrows(grob=lineToGrob(x=0.5, y=unit(0.5, "npc") + 0.5*boxheight),
            angle=10, length=unit(3, "mm"))
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.move.to(x=0.5, y=unit(0.5, "npc") - 0.5*boxheight)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
grid.arrows(grob=lineToGrob(x=0.5, y=unit(0.5, "npc") + 0.5*boxheight),
            angle=10, length=unit(3, "mm"))
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.move.to(x=0.5, y=unit(0.5, "npc") - 0.5*boxheight)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=3))
grid.arrows(grob=lineToGrob(x=0.5, y=unit(0.5, "npc") + 0.5*boxheight),
            angle=10, length=unit(3, "mm"))
popViewport()



}
figure6.4 <- function() {
grid.rect(gp=gpar(col="grey"))
pushViewport(viewport(just="bottom", gp=gpar(cex=0.7)))
grid.xaxis(name="axis1", at=1:4/5)
childNames(grid.get("axis1"))

grid.edit("axis1", at=1:3/4)

grid.edit(gPath("axis1", "labels"), rot=45)

popViewport()



}
figure6.5 <- function() {
tg <- textGrob("sample text")
rg <- rectGrob(width=1.1*grobWidth(tg), 
               height=1.3*grobHeight(tg))
boxedText <- gTree(children=gList(tg, rg))



pushViewport(viewport(layout=grid.layout(1, 7,
                                         heights=unit(1.25, "inches"),
                                         widths=unit(rep(c(1, 1.25), length=7),
                                                     rep(c("null", "inches"),
                                                           length=7)))))
pushViewport(viewport(layout.pos.col=2))
grid.rect(gp=gpar(col="grey"))
grid.draw(boxedText)

popViewport()
pushViewport(viewport(layout.pos.col=4))
grid.rect(gp=gpar(col="grey"))
grid.draw(editGrob(boxedText, gp=gpar(col="grey")))

popViewport()
pushViewport(viewport(layout.pos.col=6))
grid.rect(gp=gpar(col="grey"))
grid.draw(editGrob(boxedText, vp=viewport(angle=45),
                   gp=gpar(fontsize=18)))
              
popViewport()
popViewport()
                             


}
figure6.6 <- function() {
label <- textGrob("A\nPlot\nLabel ",
                  x=0, just="left")
x <- seq(0.1, 0.9, length=50)
y <- runif(50, 0.1, 0.9)
gplot <- 
  gTree(
    children=gList(rectGrob(gp=gpar(col="grey60",
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



grid.rect(gp=gpar(col="grey60", fill="grey90"))
pushViewport(viewport(layout=layout))
pushViewport(viewport(layout.pos.col=2))
grid.draw(label)
popViewport()
pushViewport(viewport(layout.pos.col=1))
grid.draw(gplot)
popViewport(2)




}
figure6.7 <- function() {
tg1 <- textGrob("Sample")
rg1 <- rectGrob(x=rep(0.5, 2),
                width=1.1*grobWidth(tg1), 
                height=1.3*grobHeight(tg1),
                gp=gpar(col=c("grey60", "white"), 
                        lwd=c(3, 1)))



pushViewport(viewport(layout=grid.layout(1, 7,
                                         heights=unit(1.25, "inches"),
                                         widths=unit(rep(c(1, 1.25), length=7),
                                                     rep(c("null", "inches"),
                                                           length=7)))))
pushViewport(viewport(layout.pos.col=2))
grid.rect(gp=gpar(col="grey"))
grid.draw(tg1)
grid.draw(rg1)

popViewport()
pushViewport(viewport(layout.pos.col=4))
grid.rect(gp=gpar(col="grey"))
pushViewport(viewport(gp=gpar(cex=2)))
grid.draw(tg1)
grid.draw(rg1)
popViewport()

popViewport()
pushViewport(viewport(layout.pos.col=6))
grid.rect(gp=gpar(col="grey"))
pushViewport(viewport(gp=gpar(cex=2)))
grid.draw(tg1)
popViewport()
grid.draw(rg1)

popViewport()
popViewport()



}
figure6.8 <- function() {
tg1 <- textGrob("Sample", name="tg1")
rg1 <- rectGrob(width=1.1*grobWidth("tg1"), 
                height=1.3*grobHeight("tg1"),
                gp=gpar(col="grey60", lwd=3))
rg2 <- rectGrob(width=1.1*grobWidth(tg1), 
                height=1.3*grobHeight(tg1),
                gp=gpar(col="white"))



pushViewport(viewport(gp=gpar(cex=1.5)))
grid.rect(gp=gpar(col="grey"))
grid.draw(tg1)
grid.draw(rg1)
grid.draw(rg2)

grid.edit("tg1", grep=TRUE, global=TRUE, 
          label="Different text")

popViewport()



}
figure6.9 <- function() {
grid.rect(gp=gpar(col="grey"))
grid.circle(r=0.3, gp=gpar(fill="grey80"), 
            name="mycircle")
grid.edit("mycircle", gp=gpar(lwd=5))
grid.edit("mycircle", gp=gpar(lty="dashed"))




}
figure6.10 <- function() {
angle <- seq(0, 2*pi, length=21)[-21]
x <- cos(angle)
y <- sin(angle)



trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
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
figure1.12 <- function() {

#
# Comment:
#
# Makes use of the roundRect grob defined in the RGraphics package
#

require(RGraphics)

label <- function(label, row, col, title=FALSE, box=TRUE, tcol="black",
                  fill=NA, lwd=1) {
  if (title) {
    face <- "bold"
    cex <- 0.8
  } else {
    face <- "plain"
    cex <- 0.8
  }
  pushViewport(viewport(layout.pos.row=row,
                         layout.pos.col=col))
  if (box)
    grid.roundRect(w=unit(0.82, "inches"), h=unit(1.2, "lines"),
                   r=unit(0.2, "snpc"), 
                   gp=gpar(col=tcol, lwd=lwd, fill=fill))
  grid.text(label, gp=gpar(fontface=face, cex=cex, col=tcol))
  popViewport()
}

arrow <- function(row1, col1, row2, col2, col="black") {
  pushViewport(viewport(layout.pos.row=row1,
                         layout.pos.col=col1))
  grid.move.to(x=0.5, y=unit(0.5, "npc") - unit(0.6, "lines"))
  popViewport()
  pushViewport(viewport(layout.pos.row=row2,
                         layout.pos.col=col2))
  grid.arrows(grob=lineToGrob(x=0.5,
                y=unit(0.5, "npc") + unit(0.6, "lines")),
              angle=10, type="closed", 
              length=unit(0.15, "inches"), gp=gpar(col=col, fill=col))
  popViewport()
} 

pushViewport(viewport(width=unit(4.9, "inches"),
                      layout=grid.layout(7, 5,
                         heights=unit(c(3,.7,3,.7,2,.7,3),
                           c("lines", "inches",
                             "lines", "inches",
                             "lines", "inches",
                             "lines")))
                       ))

pushViewport(viewport(layout.pos.row=1,
                       layout.pos.col=2:5))
grid.roundRect(gp=gpar(lty="dashed"), r=unit(0.1, "snpc"))
popViewport()

label("Graphics\nPackages", 1, 1, title=TRUE, box=FALSE)
label("lattice", 1, 4, lwd=3, fill="grey80")
label("...", 1, 5)
label("maps", 1, 2)
label("...", 1, 3)
arrow(1, 2, 3, 3)
arrow(1, 3, 3, 3)
arrow(1, 4, 3, 4)
arrow(1, 5, 3, 4)

pushViewport(viewport(layout.pos.row=3,
                       layout.pos.col=2:5))
grid.roundRect(gp=gpar(lty="dashed"), r=unit(0.1, "snpc"))
popViewport()

label("Graphics\nSystems", 3, 1, title=TRUE, box=FALSE)
label("graphics", 3, 3, lwd=3, fill="grey80")
label("grid", 3, 4, lwd=3, fill="grey80")
arrow(3, 3, 5, 3:4)
arrow(3, 4, 5, 3:4)

label("Graphics\nEngine\n&\nDevices", 5, 1, title=TRUE, box=FALSE)
label("grDevices", 5, 3:4, lwd=3, fill="grey80")
# arrow(5, 3:4, 7, 2)
# arrow(5, 3:4, 7, 3)
# arrow(5, 3:4, 7, 4)
# arrow(5, 3:4, 7, 5)

pushViewport(viewport(layout.pos.row=7,
                       layout.pos.col=2:5))
grid.roundRect(gp=gpar(lty="dashed"), r=unit(0.1, "snpc"))
popViewport()

label("Graphics\nDevice\nPackages", 7, 1, title=TRUE, box=FALSE)
label("gtkDevice", 7, 3)
label("...", 7, 4)
      
popViewport()



}
figure2.1 <- function() {
par(mfrow=c(2, 2), cex=0.6, mar=c(4, 4, 1, 1))
y <- rnorm(20)
plot(y, type="p")
plot(y, type="l")
plot(y, type="b")
plot(y, type="h")




}
figure2.2 <- function() {
  par(mfrow=c(2, 2), cex=0.6, mar=c(4, 4, 4, 2), mex=0.8)
  plot(lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings),
    id.n=1)



}
figure2.3 <- function() {
library(cluster)



subset <- sample(1:150, 20)
cS <- as.character(Sp <- iris$Species[subset])
cS[Sp == "setosa"] <- "S"
cS[Sp == "versicolor"] <- "V"
cS[Sp == "virginica"] <- "g"
ai <- agnes(iris[subset, 1:4])
par(mfrow=c(2, 1), cex=0.5, pty="s")
plot(ai, which=1, col=c("grey90", "grey"), labels = cS)
plot(ai, which=2, labels = cS)



}
figure2.4 <- function() {
par(mfrow=c(2, 2), mar=c(2.5, 2, 1, 1), cex=0.6)
boxplot(decrease ~ treatment, data = OrchardSprays,
        log = "y", col="light grey")
boxplot(decrease ~ treatment, data = OrchardSprays,
        log = "y", col="light grey", 
        boxwex=0.5)

par(las=2, xpd=NA)
barplot(VADeaths[1:2,], angle = c(45, 135), 
        density = 20, col = "grey",
        names=c("RM", "RF", "UM", "UF"))
barplot(VADeaths[1:2,], angle = c(45, 135), 
        density = 20, col = "grey",
        names=c("RM", "RF", "UM", "UF"),
        horiz=TRUE)




}
figure2.5 <- function() {
par(mfrow=c(2, 2), mar=c(2, 2, 1, 1), cex=0.6)
y <- rnorm(20)
plot(y, type="l", lwd=3)
plot(y, type="l", col="grey")
plot(y, type="l", lty="dashed")
plot(y, type="l", ylim=c(-4, 4))




}
figure2.6 <- function() {
par(mfrow=c(2, 2))
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
# Don't draw the grid lines :  border = NA
par(mar=rep(0, 4))
persp(x, y, z, theta = 135, phi = 30, col = "light grey", scale = FALSE,
      ltheta = -120, shade = 0.75, border = NA, box = FALSE)
mtext("persp()", side=3, line=-2)
par(mar=c(3, 3, 2, 0.5))
# Note that  example(trees)  shows more sensible plots!
N <- nrow(trees)
attach(trees)
# Girth is diameter in inches
symbols(Height, Volume, circles=Girth/24, inches=FALSE,
        main="", xlab="", ylab="", bg=grey(Girth/max(Girth)))
mtext("symbols()", side=3, line=0.5)
par(mar=rep(0.5, 4))
contour(x, y, z, asp=1, labcex=0.35, axes=FALSE)
rect(0, 0, 870, 620)
mtext("contour()", side=3, line=-1.5)
image(x, y, z, asp=1, col=grey(0.5 + 1:12/24), xlab="", ylab="", axes=FALSE)
rect(min(x)-5, min(y)-5, max(x)+5, max(y)+5)
mtext("image()", side=3, line=-1.5)



}
figure2.7 <- function() {
# Awful way to position multiple plots here (using oma)
# Won't lay things out nicely on different sized device or
# different output format (i.e. where line heights differ)
par(cex=0.6)
pairs(iris[1:4], oma=c(18, 4, 4, 4), 
      panel=function(x, y, ...) {
	      points(x, y, lwd=0.1,
              pch = ".") }) 
par(cex=1)

par(new=TRUE)
par(omi=c(0, 0, 4.7, 0), mfrow=c(1, 2), mfg=c(1, 1), xpd=NA)
par(mar=c(1, 1, 0, 1))
palette(grey(0.5 + 1:8/24))
stars(mtcars[1:8, 1:7], len = 0.8, cex=0.5,
      draw.segments = TRUE, xpd=NA)

par(mar=c(0, 1, 1, 1))
# Terrible hack to add empty lines to data set names
# to try to get labelling right in small size
dm <- dimnames(Titanic)
dm$Sex <- c("Male\n\n", "Female\n\n")
dm$Survived <- c("No\n", "Yes\n")
dm$Age <- c("Child\n", "Adult\n")
dimnames(Titanic) <- dm
mosaicplot(~ Sex + Age + Survived, data = Titanic, off=rep(5, 3),
           ylab="", main="", 
           color = c("light grey", "dark grey"))



}
figure2.8 <- function() {
par(mfrow=c(2, 2))
par(mar=c(7, 0, 3, 1))
par(mex=0.7)
dotchart(t(VADeaths[1:3,]), xlim = c(0,40), cex=0.6,
         labels=c("RM", "RF", "UM", "UF"))

par(mex=1)
par(mar=c(5, 3, 2, 1))
sunflowerplot(x=sort(round(rnorm(100))), y= round(rnorm(100),0),
             xlab="", ylab="", xlim=c(-3, 3), ylim=c(-3, 3), cex=0.5,
             size=1/12, seg.lwd=1, seg.col="grey",
             axes=FALSE)
axis(1, at=seq(-3, 3, 3))
axis(2, at=seq(-3, 3, 3))

hc <- hclust(dist(USArrests), "ave")
dend1 <- as.dendrogram(hc)
dend2 <- cut(dend1, h=70)
par(cex=0.7)
par(mar=c(1, 0, 2, 5.5))
#  dend2$lower is *NOT* a dendrogram, but a list of .. :
plot(dend2$lower[[3]], 
  horiz = TRUE, type = "tr", axes=FALSE, cex=0.8)
par(mar=c(6, 0, 2, 0))
# "inner" and "leaf" edges in different type & color :
plot(dend2$lower[[2]], 
     edgePar = list(col=c("black", "grey")), edge.root=TRUE, 
     axes=FALSE, cex=0.8)



}
figure4.1 <- function() {
trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
trellis.par.set(list(dot.symbol=list(pch=1)))
print(
xyplot(lat ~ long, data=quakes, pch=".")

)



}
figure4.2 <- function() {
tplot <- xyplot(lat ~ long, data=quakes, pch=".")



trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
trellis.par.set(list(dot.symbol=list(pch=1)))
print(
update(tplot, 
       main="Earthquakes in the Pacific Ocean\n(since 1964)")

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
angle <- seq(0, 2*pi, length=21)[-21]
xx <- cos(angle)
yy <- sin(angle)
gg <- factor(rep(1:2, each=10))

aaa <- seq(0, pi, length=10)
xxx <- rep(aaa, 10)
yyy <- rep(aaa, each=10)
zzz <- sin(xxx) + sin(yyy)


doplot <- function(name, ...) {
  do.call(name, 
          list(..., scales=list(draw=FALSE), xlab=NULL, ylab=NULL,
               strip=function(which.panel, ...) { 
                       grid.rect(gp=gpar(fill="grey90")); grid.text(name) 
                     }))
}
plot <- vector("list", 15)
plot[[1]] <- doplot("barchart", y ~ g | 1)
plot[[2]] <- doplot("bwplot", yy ~ gg | 1, 
                    par.settings=list(box.umbrella=list(lwd=0.5)))
plot[[3]] <- doplot("densityplot", ~ yy | 1)
plot[[4]] <- doplot("dotplot", y ~ g | 1)
plot[[5]] <- doplot("histogram", ~ yy | 1)
plot[[6]] <- doplot("qqmath", ~ yy | 1)
plot[[7]] <- doplot("stripplot", yy ~ gg | 1)
plot[[8]] <- doplot("qq", gg ~ yy | 1)
plot[[9]] <- doplot("xyplot", xx ~ yy | 1)
plot[[10]] <- doplot("levelplot", zzz ~ xxx + yyy | 1, colorkey=FALSE)
plot[[11]] <- doplot("contourplot", zzz ~ xxx + yyy | 1, labels=FALSE, cuts=8)
plot[[12]] <- doplot("cloud", zzz ~ xxx + yyy | 1, zlab=NULL, zoom=0.9, 
                     par.settings=list(box.3d=list(lwd=0.01)))
plot[[13]] <- doplot("wireframe", zzz ~ xxx + yyy | 1, zlab=NULL, zoom=0.9,
                     drape=TRUE, par.settings=list(box.3d=list(lwd=0.01)),
                     colorkey=FALSE)
plot[[14]] <- doplot("splom", ~ data.frame(x=xx[1:10], y=yy[1:10]) | 1, 
                     pscales=0)
plot[[15]] <- doplot("parallel", ~ data.frame(x=xx[1:10], y=yy[1:10]) | 1)

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
depthgroup <- equal.count(quakes$depth, number=3, overlap=0)



trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
trellis.par.set(list(dot.symbol=list(pch=1)))
trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
print(
xyplot(lat ~ long | depthgroup, data=quakes, pch=".")

)



}
figure4.5 <- function() {
depthgroup <- equal.count(quakes$depth, number=3, overlap=0)



magnitude <- equal.count(quakes$mag, number=2, overlap=0)



trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
print(
xyplot(lat ~ long | depthgroup * magnitude,
       data=quakes,
       main="Fiji Earthquakes",
       ylab="latitude", xlab="longitude",
       pch=".",
       scales=list(x=list(alternating=c(1, 1, 1))),
       between=list(y=1),
       par.strip.text=list(cex=0.7),
       par.settings=list(axis.text=list(cex=0.7)))

)



}
figure4.6 <- function() {
trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
trellis.par.set(list(fontsize=list(text=9, points=8)))
show.settings()



}
figure4.7 <- function() {
depthgroup <- equal.count(quakes$depth, number=3, overlap=0)



trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
trellis.par.set(list(dot.symbol=list(pch=1)))
print(
xyplot(lat ~ long | depthgroup, data=quakes, pch=".",
       layout=c(1, 3), aspect=1, index.cond=list(3:1))

)



}
figure4.8 <- function() {
trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
trellis.par.set(list(fontsize=list(text=10)))
spots <- by(sunspots, gl(235, 12, lab=1749:1983), mean)
plot1 <- xyplot(spots ~ 1749:1983, xlab="", type="l",
                main="Average Yearly Sunspots", 
                scales=list(x=list(alternating=2)))
plot2 <- xyplot(spots ~ 1749:1983, xlab="Year", type="l")
print(plot1, position=c(0, 0.2, 1, 1), more=TRUE)
print(plot2, position=c(0, 0, 1, 0.33))




}
figure4.9 <- function() {
trellis.par.set(theme = canonical.theme("postscript", col=FALSE))
myPanel <- function(x, y, subscripts, ...) {
  panel.xyplot(x, y, ...)
  ltext(x - 1, y + 1, letters[subscripts], cex=0.5)
}

myStrip <- function(which.panel, ...) {
  font <- rep(1, 2)
  font[which.panel] <- 2
  col=rep("grey", 2)
  col[which.panel] <- "black"
  llines(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
  ltext(c(0.33, 0.66), rep(0.5, 2), 1:2, 
        font=font, col=col)
}

myPrePanel <- function(x, y, ...) {
  list(xlim=c(min(x) - 1, max(x) + 1),
       ylim=c(min(y) - 1, max(y) + 1))
}

X <- 1:20
Y <- 1:20
G <- factor(rep(1:2, 10))

print(
xyplot(X ~ Y | G, aspect=1, layout=c(1, 2),
       panel=myPanel, strip=myStrip,
       prepanel=myPrePanel)

)



}
