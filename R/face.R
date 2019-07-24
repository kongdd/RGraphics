#' @name face
#' @title Draw a face
#' 
#' @description
#' Draws a face, consisting of a rectangle for the border, circles for eyes, 
#' and a line for the mouth.
#' 
#' @details
#' The functions `faceA` and `faceB` are graphics functions to be used for their 
#' side effect of producing graphical output. The functions `faceC`, `faceD`, 
#' and `faceE` return a grob representing a face (and produce no output).
#' 
#' @param x,y,width,height Numeric values or unit objects specifying the 
#' location and size of the face.
NULL

#' @rdname face
#' @export
faceA <- function(x, y, width, height) {
    pushViewport(viewport(x=x, y=y, 
                          width=width, height=height))
    grid.rect()
    grid.circle(x=c(0.25, 0.75), y=0.75, r=0.1)
    grid.lines(x=c(0.33, 0.67), y=0.25)
    popViewport()
}

#' @rdname face
#' @export
faceB <- function(x, y, width, height) {
    pushViewport(viewport(x=x, y=y, 
                          width=width, height=height))
    grid.draw(rectGrob())
    grid.draw(circleGrob(x=c(0.25, 0.75), y=0.75, r=0.1))
    grid.draw(linesGrob(x=c(0.33, 0.67), y=0.25))
    popViewport()
}

#' @rdname face
#' @export
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

#' @rdname face
#' @export
faceD <- function(x, y, width, height) {
    grid.grabExpr({
        pushViewport(viewport(x=x, y=y,
                              width=width, 
                              height=height))
        grid.rect()
        grid.circle(x=c(0.25, 0.75), 
                    y=0.75, r=0.1)
        grid.lines(x=c(0.33, 0.67), y=0.25)
        popViewport()
    })
}

#' @rdname face
#' @export
faceE <- function(x, y, width, height) {
    grob(x=x, y=y, width=width, height=height, cl="face")
}

#' @export
drawDetails.face <- function(x, recording) {
    pushViewport(viewport(x=x$x, y=x$y,
                          width=x$width, height=x$height))
    grid.rect()
    grid.circle(x=c(0.25, 0.75), y=0.75, r=0.1)
    grid.lines(x=c(0.33, 0.67), y=0.25)
    popViewport()  
}
