#' @title Draw a Map of Australia
#' 
#' Draws a map of Australia (or part thereof).
#' 
#' @param ozRegion An object created using the `ozRegion` function from the 
#' \pkg{oz} package.
#' 
#' @examples
#' \dontrun{
#' grid.ozFun(oz::ozRegion())
#' }
#' @export
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

#' ozGrob
#' 
#' @inheritParams grid.ozFun
#' @inheritParams imageGrob
#' 
#' @export
ozGrob <- function(ozRegion, name=NULL, gp=NULL, vp=NULL) {
    gTree(ozRegion=ozRegion, name=name, gp=gp, vp=vp, 
          childrenvp=makeOzViewports(ozRegion), 
          children=makeOzLines(ozRegion), 
          cl="ozGrob")
}

#' @export
validDetails.ozGrob <- function(x) {
    if (!inherits(x$ozRegion, "ozRegion"))
        stop("Invalid ozRegion")
    x
}

#' @param ... Arguments to be passed to [ozGrob].
#' @rdname ozGrob
#' @export
grid.ozGrob <- function(...) {
    grid.draw(ozGrob(...))
}

#' Create an Image on a Map of Australia
#' 
#' Creates a map of Australia (or part thereof), plus an [imageGrob] positioned 
#' relative to the map.
#' 
#' @param mapLong,mapLat Longitude and latitude ranges describing the area of 
#' Australia to create.
#' @param imageLong,imageLat Longitude and latitude ranges describing the area 
#' that the image should occupy.
#' @param cols Colors for the image cells.
#' 
#' @export
ozImage <- function(mapLong, mapLat, 
                    imageLong, imageLat, cols) 
{
    grob(mapLong=mapLong, mapLat=mapLat, 
         imageLong=imageLong, imageLat=imageLat, cols=cols,
         cl="ozImage")  
}

#' ozKey
#' 
#' Creates a map of Australia with a rectangle bounding a specified region; 
#' designed to provide a key for an ozImage.
#' 
#' @param x,y,width,height,just The location and size of the key within the 
#' current viewport.
#' @inheritParams ozImage
#' 
#' @export
ozKey <- function(x, y, width, height, just, 
                  mapLong, mapLat) 
{
    gTree(childrenvp=viewport(name="ozkeyframe",
                              x=x, y=y, just=just,
                              width=width, height=height),
          children=gList(ozGrob(oz::ozRegion(), vp="ozkeyframe",
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


#' @export
drawDetails.ozImage <- function(x, recording) { 
    grid.draw(ozGrob(oz::ozRegion(xlim=x$mapLong, 
                                  ylim=x$mapLat))) 
    depth <- downViewport(vpPath("ozlay", "ozvp"))
    pushViewport(viewport(y=min(x$imageLat), 
                          height=diff(range(x$imageLat)), 
                          x=max(x$imageLong), 
                          width=diff(range(x$imageLong)),
                          default.units="native", 
                          just=c("right", "bottom")))
    grid.draw(imageGrob(50, 50, cols=x$col)) 
    popViewport()
    upViewport(depth)
} 

#' @export
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

#' @export
editDetails.ozGrob <- function(x, specs) {
    if ("ozRegion" %in% names(specs)) {
        x$childrenvp <- makeOzViewports(x$ozRegion)
        x <- setChildren(x, makeOzLines(x$ozRegion))
    }
    x
}
