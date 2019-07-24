figure19.1 <- function() {
    midpts <- barplot(1:10, col="gray90", axes=FALSE)
    axis(2)
    axis(1, at=midpts, labels=FALSE)

    vps <- gridBase::baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)

    grid.text(c("one", "two", "three", "four", "five",
                "six", "seven", "eight", "nine", "ten"), 
              x=unit(midpts, "native"), y=unit(-1, "lines"),
              just="right", rot=60)
    popViewport(3)
}

figure19.2 <- function() {
    hc <- hclust(dist(USArrests), "ave")
    dend1 <- as.dendrogram(hc)
    dend2 <- cut(dend1, h=70)

    x <- 1:4
    y <- 1:4
    height <- factor(round(sapply(dend2$lower, 
                                  attr, "height")))

    space <- 1.2 * max(stringWidth(rownames(USArrests)))
    dendpanel <- function(x, y, subscripts, ...) {
        pushViewport(viewport(gp=gpar(fontsize=8)),
                     viewport(y=unit(0.95, "npc"), width=0.9,
                              height=unit(0.95, "npc") - space,
                              just="top"))
        par(plt=gridBase::gridPLT(), new=TRUE, ps=8)
        plot(dend2$lower[[subscripts]], axes=FALSE)
        popViewport(2)
    }

    trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
    plot.new()
    print(xyplot(y ~ x | height, subscripts=TRUE, 
                 xlab="", ylab="",
                 strip=strip.custom(style=4), 
                 scales=list(draw=FALSE), 
                 panel=dendpanel),
          newpage=FALSE)
}

