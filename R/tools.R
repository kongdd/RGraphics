#' A Traditional Graphics Function Template
#' 
#' A template that provides a starting point for writing a new traditional 
#' graphics function.
#' 
#' Type plot.newclass to see the body of this template.
#' 
#' @export
plot.newclass <- function(
    x, y=NULL, 
    main="", sub="",
    xlim=NULL, ylim=NULL,
    axes=TRUE, ann=par("ann"),
    col=par("col"),
    ...) 
{
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
        title(main=main, sub=sub, xlab=xy$xlab, ylab=xy$ylab, ...)
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
             gp=gpar(col=NA, fill=cols),
             name="image")
}

#' @import foreach
check_dir <- function(path){
    foreach(path_i = unique(path)) %do% {
        if (!dir.exists(path_i)){
            dir.create(path_i, recursive = TRUE)
        }    
    }
    path
}
