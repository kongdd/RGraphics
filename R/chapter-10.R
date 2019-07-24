
figure10.1 <- function() {
    names <- c("blank", "solid", "dashed", "dotted", "dotdash",
               "longdash", "twodash", "", "", "13", "F8", "431313", "22848222")
    hgap <- unit(5, "mm")
    pushViewport(viewport(layout=grid.layout(17, 5,
                                             widths=unit.c(unit(1, "strwidth", "Integer") + unit(3, "mm"), 
                                                           hgap, unit(1.5, "inches"),
                                                           hgap, max(unit(rep(1, length(names)), "strwidth", data=as.list(names))) + unit(3, "mm")),
                                             heights=unit(c(1.2, 1, 1, rep(1.2, 7), 1, 1, rep(1.2, 4), 1), "lines"))))
    pushViewport(viewport(layout.pos.col=1:5, layout.pos.row=1:17))
    grid.rect(width=1.3, height=1.3, gp=gpar(col="gray"))
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

figure10.2 <- function() {
    x <- c(.3, .7, .3)
    y <- c(.2, .5, .8)
    grid.rect(gp=gpar(col="gray"))
    grid.lines(x, y, gp=gpar(lwd=40, lineend="square",
                             linejoin="mitre", col="black"))
    grid.lines(x, y, gp=gpar(lwd=40, col="gray50"))
    # lineend="round", linejoin="round"
    grid.lines(x, y, gp=gpar(lwd=40, lineend="butt",
                             linejoin="bevel", col="gray80"))
    grid.points(x, y, default.units="npc", pch=16, gp=gpar(cex=0.5))
}

figure10.3 <- function() {
    ncol <- 6
    nrow <- 5
    grid.rect(gp=gpar(col="gray"))
    for (i in 1:nrow) {
        for (j in 1:ncol) {
            x <- unit(j/(ncol+1), "npc")
            y <- unit(i/(nrow + 1), "npc")
            pch <- (i - 1)*ncol + j - 1
            if (pch > 25) 
                pch <- c("A", "b", ".", "#")[pch - 25]
            grid.points(x + unit(3, "mm"), y, 
                        pch=pch, gp=gpar(fill="gray"))
            grid.text(pch, x - unit(3, "mm"), y, gp=gpar(col="gray"))
        }
    }
}

figure10.4 <- function() {
    h <- 0.01
    drawexpr <- function(expr, y, exprFamily="CM") {
        grid.text(paste("expression(", expr, ")", sep=""), .5, y-h, 
                  just="top", gp=gpar(fontfamily="mono", cex=0.75))
        grid.text(parse(text=expr), .5, y+h, 
                  just="bottom", gp=gpar(fontfamily=exprFamily))
    }
    drawexpr("z[i] == sqrt(x[i]^2 + y[i]^2)", 1/5)
    drawexpr("hat(beta) == (X^t * X)^{-1} * X^t * y", 2/5)
    drawexpr("bar(x) == sum(frac(x[i], n), i==1, n)", 3/5)
    drawexpr("paste(\"Temperature (\", degree, \"C) in 2003\")", 4/5,
             exprFamily="CM2")
    grid.rect(gp=gpar(col="gray", fill=NA))
}
