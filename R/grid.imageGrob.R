#' @title Draw an Image
#' 
#' Draws an array of nrow by ncol rectangles.
#' 
#' @param nrow,ncol Number of rows/columns in the image.
#' @param cols Colors for the cells of the image (will be recycled).
#' @param byrow Logical value indicating whether colors should be allocated to 
#' cells across rows or down columns.
#' 
#' @export
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

#' grid.imageGrob
#' Creates an imageGrob and then draws it.
#' 
#' @param ... Arguments to be passed to [imageGrob].
#' 
#' @export
grid.imageGrob <- function(...) {
    igt <- imageGrob(...)
    grid.draw(igt)
}

#' imageGrob
#' 
#' @inheritParams grid.imageFun
#' @param name A character name for the grob.
#' @param gp A [grid::gpar] object containing graphical parameter settings or NULL.
#' @param vp A [grid::viewport] or `NULL`.
#' 
#' @export
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

#' @export
validDetails.imageGrob <- function(x) { 
    if (!is.numeric(x$nrow) || length(x$nrow) > 1 || 
        !is.numeric(x$ncol) || length(x$ncol) > 1)
        stop("nrow and ncol must be numeric and length 1")
    if (!is.logical(x$byrow))
        stop("byrow must be logical")
    x 
} 
