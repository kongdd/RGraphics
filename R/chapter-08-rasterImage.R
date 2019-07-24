#' @name figure08
#' @title figure08
NULL

# raster image
figure8.3 <- function() {
    grays <- gray(0.5 + (rep(1:4, 4) - rep(0:3, each=4))/10)

    pushViewport(viewport(layout=grid.layout(3, 5, widths=c(1,8,2,8,1),
                                             heights=unit(c(1, 8, 1), c("null", "null", "line")))))
    pushViewport(viewport(layout.pos.col=2, 
                          layout.pos.row=2))
    grid.imageFun(4, 4, grays)

    popViewport()
    pushViewport(viewport(layout.pos.col=2, 
                          layout.pos.row=3))
    grid.text("(a)", gp=gpar(cex=0.7))
    popViewport()
    pushViewport(viewport(layout.pos.col=4,
                          layout.pos.row=2))
    grid.imageFun(4, 4, grays, byrow=FALSE)

    popViewport()
    pushViewport(viewport(layout.pos.col=4, 
                          layout.pos.row=3))
    grid.text("(b)", gp=gpar(cex=0.7))
    popViewport(2)
}

# edit raster image lwd
#' @keywords internal
#' @export
figure8.13 <- function() {
    grays <- gray(0.5 + (rep(1:4, 4) - rep(0:3, each=4))/10)

    pushViewport(viewport(layout=grid.layout(2, 1, 
                                             heights=unit(c(1, 1),
                                                          c("null", "line")),
                                             respect=TRUE)))
    pushViewport(viewport(layout.pos.row=1))
    grid.imageGrob(4, 4, grays, name="imageGrob")

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

# add xtitle for raster image
figure8.14 <- function() {
    grid.newpage()

    grays <- gray(0.5 + (rep(1:4, 4) - rep(0:3, each=4))/10)

    pushViewport(viewport(layout=grid.layout(2, 1, 
                                             heights=unit(c(1, 1),
                                                          c("null", "line")),
                                             respect=TRUE)))
    pushViewport(viewport(layout.pos.row=1))
    grid.imageGrob(4, 4, grays, name="imageGrob")
    grid.edit("imageGrob::image", gp=gpar(col="white"))    
    popViewport()

    pushViewport(viewport(layout.pos.row=2, gp=gpar(cex=0.7)))
    grid.text("(a)", name="label")
    popViewport()

    grid.edit("imageGrob", cols=rev(grays))
    # grid.remove("label")
    pushViewport(viewport(layout.pos.row=2, gp=gpar(cex=0.7)))
    grid.text("(b)", name="label")
    popViewport()
    grid.edit("imageGrob", byrow=FALSE)

    grid.remove("label")
    pushViewport(viewport(layout.pos.row=2, gp=gpar(cex=0.7)))
    grid.text("(c)", name="label")
    popViewport(2)
}

# example of `splitString`
#' @keywords internal
#' @export
figure8.20 <- function() {
    grid.newpage()
    text <- "The quick brown fox jumps over the lazy dog."
    grid.text(splitString(text), 
              x=0, y=1, just=c("left", "top")) 

    pushViewport(viewport(layout=grid.layout(2, 2)))
    pushViewport(viewport(layout.pos.col=1))
    pushViewport(viewport(width=0.5, height=0.9))
    grid.rect(gp=gpar(col="gray"))
    text <- "The quick brown fox jumps over the lazy dog."
    grid.text(splitString(text), 
              x=0, y=1, just=c("left", "top")) 

    popViewport(2)
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
    pushViewport(viewport(height=0.8))
    grid.rect(gp=gpar(col="gray"))
    splitText <- splitTextGrob(text, name="splitText")
    grid.draw(splitText)

    popViewport(2)
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
    pushViewport(viewport(height=0.8))
    grid.rect(gp=gpar(col="gray"))
    grid.draw(editGrob(splitText, gp=gpar(cex=1.5)))
    popViewport(2)
    popViewport()
}

# faces
figure8.22 <- function() {
    faceA(.5, .5, width=.1, height=.1)
    angle <- seq(0, 2*pi, length=9)[-9]
    for (i in angle) {
        x <- 0.5 + 0.3*cos(i)
        y <- 0.5 + 0.3*sin(i)
        faceA(x, y, 0.2*x, 0.2*y)
    }
    grid.rect(width=.9, height=.9, gp=gpar(col="gray", fill=NA))
}

figure8.25 <- function() {
    grid.newpage()
    grid.draw(faceC(.5, .5, .5, .5))

    grid.rect(gp=gpar(col="gray", fill=NA))
}
