# x, y, is the real corner
roundCorner <- function(num, x, y, r) {
  n <- 10*4
  t <- seq(0, 2*pi, length=n)
  cost <- cos(t)
  sint <- sin(t)
  if (num == 1) {
    xc <- x + r
    yc <- y + r
    subset <- (n/2):(3*n/4)
  } else if (num == 2) {
    xc <- x + r
    yc <- y - r
    subset <- (n/4):(n/2)
  } else if (num == 3) {
    xc <- x - r
    yc <- y - r
    subset <- 1:(n/4)
  } else if (num == 4) {
    xc <- x - r
    yc <- y + r
    subset <- (3*n/4):n
  }
  list(x=xc + (cost*r)[subset], y=yc + (sint*r)[subset])
}
# Good idea to choose r as absolute unit or "snpc"
roundRect <- function(x=unit(0.5, "npc"), 
                      y=unit(0.5, "npc"), 
                      width=unit(1, "npc"), 
                      height=unit(1, "npc"), 
                      r=unit(0.1, "snpc"),
                      just="centre",
                      name=NULL, gp=NULL, vp=NULL) {
  grob(x=x, y=y, width=width, height=height, r=r,
       name=name, gp=gp, vp=vp, cl="roundrect")
}

preDrawDetails.roundrect <- function(x) {
  pushViewport(viewport(x$x, x$y, x$width, x$height, just=x$just),
               recording=FALSE)
}

postDrawDetails.roundrect <- function(x) {
  popViewport(recording=FALSE)
}

drawDetails.roundrect <- function(x, recording) {
  left <- 0
  bottom <- 0
  right <- convertX(unit(1, "npc"), "inches", valueOnly=TRUE)
  top <- convertY(unit(1, "npc"), "inches", valueOnly=TRUE)
  r <- min(convertWidth(x$r, "inches", valueOnly=TRUE),
           convertHeight(x$r, "inches", valueOnly=TRUE))
  corner1 <- roundCorner(1, left, bottom, r)
  corner2 <- roundCorner(2, left, top, r)
  corner3 <- roundCorner(3, right, top, r)
  corner4 <- roundCorner(4, right, bottom, r)
  xx <- unit(c(left + r, right - r, corner4$x,
               right, right, corner3$x,
               right - r, left + r, corner2$x,
               left, left, corner1$x),
             "inches")
  yy <- unit(c(bottom, bottom, corner4$y,
               bottom + r, top - r, corner3$y,
               top, top, corner2$y,
               top - r, bottom + r, corner1$y),
             "inches")
  grid.draw(polygonGrob(xx, yy, gp=x$gp), recording=FALSE)
}

grid.roundRect <- function(...) {
  grid.draw(roundRect(...))
}

