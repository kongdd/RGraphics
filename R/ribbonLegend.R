calcBreaks <- function(nlevels, breaks, scale) {
    if (is.null(breaks)) {
        seq(min(scale), max(scale), diff(scale)/nlevels)
    } else {
        breaks
    }
}

#' Create a Ribbon Legend
#' 
#' Creates a ribbon legend; a vertical bar broken into several colored cells, 
#' with an axis on the right-hand side.
#' 
#' @param nlevels, breaks 
#' Number of levels to be represented in the legend, specifed either as a number 
#' of (equal-sized) levels, or the break points between levels.
#' @param cols The colors to be used for each level.
#' @param scale The range of the scale on the legend.
#' @param margin Space around the edges of the legend. 
#' @param name A character name for the grob. 
#' @param gp A `gpar` object containing graphical parameter settings or `NULL`. 
#' @param vp A viewport or `NULL`.
#' 
#' @keywords dplot
#' @export
ribbonLegend <- function(nlevels=NULL, breaks=NULL, cols, 
                         scale=range(breaks), 
                         margin=unit(0.5, "line"), 
                         gp=NULL, vp=NULL, name=NULL) {
    gTree(
        nlevels=nlevels, breaks=breaks, cols=cols, scale=scale, 
        children=ribbonKids(nlevels, breaks, cols, scale),
        childrenvp=ribbonVps(nlevels, breaks, margin, scale),
        gp=gp, vp=vp, name=name, cl="ribbonLegend")
}

ribbonVps <- function(nlevels, breaks, margin, scale) {
    breaks <- format(signif(calcBreaks(nlevels, breaks, scale), 3))
    vpTree(
        viewport(name="layout", layout=
                     grid.layout(3, 4,
                                 widths=unit.c(margin, unit(1, "line"),
                                               max(unit(0.8, "line") + stringWidth(breaks)), margin),
                                 heights=unit.c(margin, unit(1, "null"), margin))),
        vpList(viewport(layout.pos.col=2, layout.pos.row=2,
                        yscale=scale, name="ribbon"),
               viewport(layout.pos.col=3, layout.pos.row=2,
                        yscale=scale, name="labels")))
}

ribbonKids <- function(nlevels, breaks, cols, scale) {
    breaks  <- calcBreaks(nlevels, breaks, scale)
    nb      <- length(breaks)
    tickloc <- breaks[-c(1, nb)]
    gList(rectGrob(y=unit(breaks[-1], "native"), 
                   height=unit(diff(breaks), "native"),
                   just="top", gp=gpar(fill=cols),
                   vp=vpPath("layout", "ribbon")),
          segmentsGrob(x1=unit(0.5, "line"),
                       y0=unit(tickloc, "native"),
                       y1=unit(tickloc, "native"),
                       vp=vpPath("layout", "labels")),
          textGrob(x=unit(0.8, "line"),
                   y=unit(tickloc, "native"),
                   just="left", 
                   label=format(signif(tickloc, 3)),
                   vp=vpPath("layout", "labels")))
}

widthDetails.ribbonLegend <- function(x) { 
    sum(layout.widths(viewport.layout(x$childrenvp[[1]]))) 
} 
